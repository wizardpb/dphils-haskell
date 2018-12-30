module Logger (startLogger, LogFunc ) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan(readChan, writeChan, newChan)
import System.Console.ANSI

data LogMsg = LogMsg Int String

logMsg :: Chan LogMsg -> Int -> String -> IO ()
logMsg logCh philId msg = do
  writeChan logCh $ LogMsg philId msg

logger :: Int -> Chan LogMsg -> IO ()
logger cmdLine logCh = forever $ do
  (LogMsg id msg) <- readChan logCh
  setCursorPosition id 0; clearLine
  putStr msg
  setCursorPosition cmdLine 0

type LogFunc = Int -> String -> IO ()

startLogger :: Int -> IO (ThreadId , LogFunc)
startLogger cmdLine = do
  ch <- newChan
  logId <- forkIO $ logger cmdLine ch
  return (logId, logMsg ch)

