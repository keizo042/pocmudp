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

newManager :: String -> Int -> IO Manager
newManager host port = do
    (sock, addr) <- newSocket host $ fromIntegral port
    conns <- newMVar M.empty
    connIds <- newMVar []
    addr2connIds <- newMVar M.empty
    next <- newChan
    tx <- newChan
    rx <- newChan
    return $ Manager sock addr conns connIds addr2connIds next tx rx
    where
      newSocket :: String -> PortNumber -> IO (Socket, SockAddr)
      newSocket host port = do
        -- TODO: suppport IPv6
        sock <- socket AF_INET Datagram defaultProtocol
        let addr = (SockAddrInet port iNADDR_ANY)
        bind sock addr
        return (sock, addr)


closeManager :: Manager -> IO ()
closeManager mgr = close (managerSocket mgr)

newConnectionId :: Manager -> IO ConnectionId
newConnectionId mgr = do
    list <- readMVar (managerConnectionIds mgr)
    return $ getUnusedId list
    where
      getUnusedId [] = 1
      getUnusedId (x:xs) = getUnusedId' x xs
        where
          getUnusedId' x [] = x+1
          getUnusedId' x (y:ys)
            | (x > 2^16) = 2^16
            | (x + 1 == y) = getUnusedId' y ys
