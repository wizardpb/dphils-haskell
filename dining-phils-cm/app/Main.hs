module Main where

import Control.Concurrent
import Control.Concurrent.Chan
import Philosopher

main :: IO ()
main = do
  channels <- mapM (\_ -> newChan) philosophers

  let
    idPhilosophers = map runPhilosopher philIds
    namedPhilosophers = zipWith ($) idPhilosophers philosophers
    chanTriples = zip3 channels (tail . cycle $ channels) (tail . tail . cycle $ channels)
    ioPhilosophers = zipWith ($) namedPhilosophers chanTriples
    initialStates = map initialState philIds
    philFuncs = zipWith ($) ioPhilosophers initialStates

  threadIds <- mapM forkIO philFuncs

  mapM startPhil channels

  getLine

  mapM killThread threadIds
  return ()