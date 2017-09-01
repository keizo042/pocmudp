module Network.MUDP.Manager
  (
   newManager
  ,closeManager
  ,newConnectionId
  )
  where

import qualified Data.Map as M

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar

import Network.MUDP.Types
import Network.Socket

newManager :: String -> String -> IO Manager
newManager host port = do
    (sock, addr) <- newSocket host port
    conns <- newMVar M.empty
    connIds <- newMVar []
    addr2connIds <- newMVar M.empty
    tx <- newChan
    rx <- newChan
    return $ Manager sock addr conns connIds addr2connIds tx rx
    where
      newSocket :: String -> String -> IO (Socket, SockAddr)
      newSocket host port = do
        let hints = defaultHints { addrSocketType = Datagram, addrProtocol = 17 }
        info <-  head  <$> getAddrInfo (Just hints) (Just host) (Just port)
        sock <- socket (addrFamily info) Datagram defaultProtocol
        return (sock, addrAddress info)


closeManager :: Manager -> IO ()
closeManager mgr = close (managerSocket mgr)

newConnectionId :: Manager -> IO ConnectionId
newConnectionId mgr = undefined
