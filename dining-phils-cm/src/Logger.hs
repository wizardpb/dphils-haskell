module Logger (startLogger, ) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan(readChan, writeChan, newChan)

data LogMsg = LogMsg Int String

logMsg logCh philId msg = do
  writeChan logCh $ LogMsg philId msg

logger logCh = forever $ do
  (LogMsg id msg) <- readChan logCh
  putMsg id msg

putMsg id msg = do
    let ctlStr = "\ESC[" ++ (show $ id + 1) ++ ";0H\ESC[2K"
    putStr $ ctlStr ++ msg

startLogger = do
  ch <- newChan
  logId <- forkIO $ logger ch
  return (logId, logMsg ch)

