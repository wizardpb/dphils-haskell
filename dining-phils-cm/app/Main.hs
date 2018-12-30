module Main where

import Control.Concurrent
import Control.Concurrent.Chan
import System.Console.ANSI

import Philosopher
import Logger

main :: IO ()
main = do
  channels <- mapM (\_ -> newChan) philosophers
  (logId, logger) <- startLogger (philCount + 3)

  let
    idPhilosophers = map runPhilosopher philIds
    namedPhilosophers = zipWith ($) idPhilosophers philosophers
    chanTriples = zip3 channels (tail . cycle $ channels) (tail . tail . cycle $ channels)
    ioPhilosophers = zipWith ($) namedPhilosophers chanTriples
    initialStates = map initialState philIds
    philFuncs = zipWith ($) ioPhilosophers initialStates

  clearScreen
  threadIds <- mapM (\pf -> forkIO $ pf logger) philFuncs
  mapM startPhil channels

  getLine

  mapM killThread threadIds
  return ()