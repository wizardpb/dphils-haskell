module Logger (startLogger, LogFunc ) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan(readChan, writeChan, newChan)

data LogMsg = LogMsg Int String

logMsg :: Chan LogMsg -> Int -> String -> IO ()
logMsg logCh philId msg = do
  writeChan logCh $ LogMsg philId msg

logger :: Chan LogMsg -> IO ()
logger logCh = forever $ do
  (LogMsg id msg) <- readChan logCh
  putMsg id msg

putMsg :: Int -> String -> IO ()
putMsg id msg = do
    let ctlStr = "\ESC[" ++ (show $ id + 1) ++ ";0H\ESC[2K"
    putStr $ ctlStr ++ msg

type LogFunc = Int -> String -> IO ()

startLogger :: IO (ThreadId , LogFunc)
startLogger = do
  ch <- newChan
  logId <- forkIO $ logger ch
  return (logId, logMsg ch)

