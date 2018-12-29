module Philosopher ( philosophers, runPhilosopher, philCount, philIds, startPhil, initialState, Message(RequestToken, XmitFork, NextState)) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan(readChan, writeChan)
import System.Random(randomRIO)
import Data.Time.Clock.System
import Data.Maybe
import GHC.Int(Int64)

data PhilState = Starting | Thinking | Hungry | Eating deriving (Eq,Show)
data ForkState = Dirty | Clean deriving (Eq, Show)
data Fork = Fork Int ForkState deriving (Eq, Show)
data ForkRecord = ForkRecord { fork :: Maybe Fork, canRequest :: Bool} deriving (Eq, Show)
data ThreadState = ThreadState {
    state :: PhilState,
    left :: ForkRecord, right :: ForkRecord
  }
  deriving (Eq, Show)

data Message = RequestToken Int | XmitFork Fork | NextState PhilState deriving Show

philosophers = ["Kant", "Marx", "Hegel", "Mill", "Heidegger"]
philCount = length philosophers
philIds = [0..philCount-1]

nextDelay :: IO Int
nextDelay = randomRIO (1,30)

sendNextState :: Int -> PhilState -> Chan Message -> IO ThreadId
sendNextState secs nextState ch = forkIO $ do
  threadDelay (secs * 100000)
  writeChan ch $ NextState nextState

initialFork forkId =
  Just (Fork forkId Dirty)

initialThreadState leftFork rightFork = ThreadState {
    state = Starting,
    left = ForkRecord { fork = leftFork, canRequest = isNothing leftFork},
    right = ForkRecord { fork = rightFork, canRequest = isNothing leftFork}
  }

nextForkId forkId = mod (forkId + 1) philCount

initialState philId
  | philId == 0 = initialThreadState (initialFork philId) (initialFork (nextForkId philId))
  | philId == 1 = initialThreadState Nothing Nothing
  | otherwise = initialThreadState (initialFork philId) Nothing

nextState philId name state @ ThreadState { state = philState, left = left, right = right} inCh = do
  msg <- readChan inCh
  delay <- nextDelay
  case msg of
      NextState Thinking -> do
        sendNextState delay Hungry inCh
        putStrLn (name ++ "is thinking...")
        return (state { state = Thinking })
      NextState Hungry -> do
        putStrLn (name ++ "is hungry...")
        return (state { state = Hungry })
      RequestToken n ->
        return (
          if n == philId
          then state { left = left { canRequest = True} }
          else state { right = right { canRequest = True} })
      XmitFork (Fork n s) ->
        return (
          if n == philId
          then state { left = left { fork = Just (Fork n s)} }
          else state { right = right { fork = Just (Fork n s)} })


requestFork forkId outCh = do
  writeChan outCh $ RequestToken forkId
  return ()

sendFork maybeFork outCh = do
  case maybeFork of
    Just (Fork n _) -> writeChan outCh $ XmitFork (Fork n Clean)
    Nothing -> error "Sending non-existent fork"
  return ()

hasFork ForkRecord { fork = fork, canRequest = _} = isJust fork

dirty ForkRecord { fork = f, canRequest = _} =
  case f of
    Just (Fork n s) -> s == Dirty
    Nothing -> error "Testing non-existent fork dirty"

forkId ForkRecord { fork = f, canRequest = _} =
  case f of
    Just (Fork n s) -> n
    Nothing -> error "Getting id non-existent fork"

actState philId name state @ ThreadState { state = philState, left = left, right = right } leftCh inCh rightCh
  | philState == Hungry && (canRequest left) && (not $ hasFork left) =
      do
        -- Request left fork
        requestFork philId leftCh
        return state { left = left {canRequest = False} }
  | philState == Hungry && (canRequest right) && (not $ hasFork right) =
      do
        -- Request right fork
        requestFork (nextForkId philId) rightCh
        return state { right = right { canRequest = False} }
  | philState == Hungry && hasFork left && hasFork right =
      do
        -- Start eating!
        delay <- nextDelay
        sendNextState delay Thinking inCh
        putStrLn (name ++ " is eating1")
        return state { state = Eating }
  | philState /= Eating && hasFork left && canRequest left && dirty left =
      do
        sendFork (fork left) leftCh
        return state { left = left { fork = Nothing } }
  | philState /= Eating && hasFork right && canRequest right && dirty right =
      do
        sendFork (fork right) rightCh
        return state { right = right { fork = Nothing}}
  | otherwise = return state


runPhilosopher :: Int -> String -> (Chan Message, Chan Message, Chan Message) -> ThreadState -> IO ()
runPhilosopher philId name (leftCh, inCh, rightCh) state = do
  ns <- nextState philId name state inCh
--  putStrLn (name ++ ": nextState=" ++ (show ns))
  as <- actState philId name ns leftCh inCh rightCh
--  putStrLn (name ++ ": act state=" ++ (show as))
  runPhilosopher philId name (leftCh, inCh, rightCh) as

startPhil ch = writeChan ch $ NextState Thinking