module Philosopher ( philosophers, runPhilosopher, philCount, philIds, startPhil, initialState, Message(RequestToken, XmitFork, NextState)) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan(readChan, writeChan)
import System.Random(randomRIO)
import Data.Time.Clock.System
import Data.Maybe
import GHC.Int(Int64)

import Logger(LogFunc)

data PhilState = Starting | Thinking | Hungry | Eating deriving (Eq,Show)
data ForkState = Dirty | Clean deriving (Eq, Show)
data Fork = Fork Int ForkState deriving (Eq, Show)
data ForkRecord = ForkRecord { fork :: Maybe Fork, canRequest :: Bool} deriving (Eq, Show)
data ThreadState = ThreadState {
    state :: PhilState,
    eatCount :: Int, eatTime :: Int,
    left :: ForkRecord, right :: ForkRecord
  }
  deriving (Eq, Show)

data Message = RequestToken Int | XmitFork Fork | NextState PhilState deriving Show

philosophers = ["Kant", "Marx", "Hegel", "Mill", "Heidegger"]
philCount = length philosophers
philIds = [0..philCount-1]

nextDelay :: IO Int
nextDelay = randomRIO (15,30)

sendNextState :: Int -> PhilState -> Chan Message -> IO ThreadId
sendNextState secs nextState ch = forkIO $ do
  threadDelay (secs * 100000)
  writeChan ch $ NextState nextState

initialFork :: Int -> Maybe Fork
initialFork forkId =
  Just (Fork forkId Dirty)

initialThreadState :: Maybe Fork -> Maybe Fork -> ThreadState
initialThreadState leftFork rightFork = ThreadState {
    state = Starting,
    eatCount = 0, eatTime = 0,
    left = ForkRecord { fork = leftFork, canRequest = isNothing leftFork},
    right = ForkRecord { fork = rightFork, canRequest = isNothing rightFork}
  }

nextForkId :: Int-> Int
nextForkId forkId = mod (forkId + 1) philCount

initialState :: Int -> ThreadState
initialState philId
  | philId == 0 = initialThreadState (initialFork 0) (initialFork 1)
  | philId == 1 = initialThreadState Nothing Nothing
  | otherwise = initialThreadState (initialFork philId) Nothing

requestFork :: Int -> Chan Message -> IO ()
requestFork forkId outCh = do
  writeChan outCh $ RequestToken forkId
  return ()

sendFork :: Maybe Fork -> Chan Message -> IO ()
sendFork maybeFork outCh = do
  case maybeFork of
    Just (Fork n _) -> writeChan outCh $ XmitFork (Fork n Clean)
    Nothing -> error "Sending non-existent fork"
  return ()

hasFork :: ForkRecord -> Bool
hasFork forkRecord =
  isJust $ fork forkRecord

forkRequested :: ForkRecord -> Bool
forkRequested forkRecord =
  canRequest forkRecord

dirty :: ForkRecord -> Bool
dirty forkRecord =
  case fork forkRecord of
    Just (Fork n s) -> s == Dirty
    Nothing -> error "Testing non-existent fork dirty"

forkId :: ForkRecord -> Int
forkId forkRecord =
  case fork forkRecord of
    Just (Fork n s) -> n
    Nothing -> error "Getting id non-existent fork"

logState :: LogFunc -> Int -> String -> ThreadState -> IO ()
logState logFunc philId name s = do
  let
    stateStr = name ++ " is " ++ (show $ state s) ++
               ", has eaten " ++ (show $ eatCount s) ++ " times, " ++
               (show $ eatTime s) ++ " seconds total"
  logFunc philId stateStr

runPhilosopher :: Int -> String -> (Chan Message, Chan Message, Chan Message) -> ThreadState -> LogFunc -> IO ()
runPhilosopher philId name (leftCh, inCh, rightCh) state logFunc = do
  let nextState state @ ThreadState { state = philState, left = left, right = right} = do
        msg <- readChan inCh
        delay <- nextDelay
        case msg of
            NextState Thinking -> do
              sendNextState delay Hungry inCh
              return state { state = Thinking }
            NextState Hungry -> do
              return state { state = Hungry }
            RequestToken n ->
              return $
                if n == philId
                then state { left = left { canRequest = True} }
                else state { right = right { canRequest = True} }
            XmitFork (Fork n s) ->
              return $
                if n == philId
                then state { left = left { fork = Just (Fork n s)} }
                else state { right = right { fork = Just (Fork n s)} }

      actState state @ ThreadState { state = philState, left = left, right = right }
        | philState == Hungry && canRequest left && (not $ hasFork left) =
            do
              -- Request left fork
              let forkId = philId
              requestFork forkId leftCh
              return state { left = left {canRequest = False} }
        | philState == Hungry && canRequest right && (not $ hasFork right) =
            do
              -- Request right fork
              let forkId = nextForkId philId
              requestFork forkId rightCh
              return state { right = right { canRequest = False} }
        | philState == Hungry && hasFork left && hasFork right =
            do
              -- Start eating!
              delay <- nextDelay
              sendNextState delay Thinking inCh
              return state {
                state = Eating,
                eatCount = (eatCount state) + 1, eatTime = (eatTime state) + delay,
                left = left { fork = Just (Fork philId Dirty)}, right = right { fork = Just (Fork (nextForkId philId) Dirty)}
              }
        | philState /= Eating && hasFork left && forkRequested left && dirty left =
            do
              let f = fork left
              sendFork f leftCh
              return state { left = left { fork = Nothing } }
        | philState /= Eating && hasFork right && forkRequested right && dirty right =
            do
              let f = fork right
              sendFork f rightCh
              return state { right = right { fork = Nothing}}
        | otherwise = return state

      recurse state = do
        logState logFunc philId name state
        ns <- nextState state
        as <- actState ns
        recurse as

  recurse state

startPhil :: Chan Message -> IO ()
startPhil ch = writeChan ch $ NextState Thinking