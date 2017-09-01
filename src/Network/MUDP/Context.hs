module Network.MUDP.Context
  (
    newContext
  , contextIsClosed
  )
  where


import Network.MUDP.Types
import Network.Socket

import qualified Data.Map as M

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar

newContext :: ConnectionId -> SockAddr -> IO Context
newContext connId addr = do
    connId' <- newMVar connId
    addr' <- newMVar addr
    sessId <- newMVar 1
    stmIds <- newMVar []
    tx <- newChan
    rx <- newChan
    closed <- newMVar False
    sessions <- newMVar M.empty
    discoard <- newChan
    next <- newChan
    return $ Context connId' addr' sessId stmIds tx rx closed sessions discoard next


contextIsClosed :: Context -> IO Bool
contextIsClosed ctx = readMVar $ contextClosed ctx
