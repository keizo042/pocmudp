module Network.MUDP.Manager
  (
   newManager
  ,closeManager
  ,newConnectionId
  )
  where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.ByteString as BS

import Control.Monad(forever)
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar


import Network.MUDP.Types
import Network.MUDP.Codec
import Network.MUDP.Context

import Network.Socket
import qualified Network.Socket.ByteString as SB

newManager :: String -> Int -> IO Manager
newManager host port = do
    (sock, addr) <- newSocket host $ fromIntegral port
    ctxInfo <- newContextInfo
    tx <- newChan
    rx <- newChan
    -- init recv, send and distribute data to connection
    recvWorker sock 1024 rx
    deliverWorker rx ctxInfo
    sendWorker sock tx
    return $ Manager sock addr ctxInfo tx rx
    where
      recvWorker :: Socket -> Int -> Chan (BS.ByteString, SockAddr) -> IO ()
      recvWorker sock pMTU rx = do
        forkIO $ forever $ do
          rdata <- SB.recvFrom sock pMTU
          writeChan rx rdata
          return ()
        return ()

      deliverWorker :: Chan  (BS.ByteString, SockAddr) -> ContextInfo -> IO ()
      deliverWorker rx info = do
        forkIO $ do
          rdata@(bs, addr) <- readChan rx
          case (decodeHeader bs) of
            -- invalid header format and drop packet
            (Left _) -> return ()

            -- recv handshake from client
            (Right (Header Handshake Nothing)) -> do
              connId  <- newConnectionId info
              ctx     <- newContext (Just connId) addr
              registerContextToInfo connId addr ctx info
              writeChan (contextRx ctx) rdata

            -- rec handshake response from  server
            (Right (Header Handshake (Just connId))) -> do
              m <- readMVar $ contextInfoAddrToConnection info
              case (M.lookup addr m) of
                (Just ctx) -> do
                  b <- isEmptyMVar (contextConnectionId ctx)
                  if b
                    then putMVar (contextConnectionId ctx) connId
                    else return ()
                Nothing -> return () -- invalid
            -- recv transport
            (Right (Header Transport (Just connId))) -> do
              conns <- readMVar (contextInfoConnections info)
              case (M.lookup connId conns) of
                (Just ctx) -> writeChan (contextRx ctx) rdata
                Nothing -> return () -- receiving invalid packet
        return ()
          where
            registerContextToInfo :: ConnectionId -> SockAddr -> Context -> ContextInfo -> IO ()
            registerContextToInfo connId addr ctx info = do
              modifyMVar_ (contextInfoConnectionIds info) (\ ids -> return $ L.insert connId ids)
              modifyMVar_ (contextInfoAddrToConnection info) (\ addrToConn -> return $ M.insert addr ctx addrToConn)
              modifyMVar_ (contextInfoConnections info) (\ conns -> return $ M.insert connId ctx conns)
              writeChan (contextInfoNextOne info) ctx
              return ()

      sendWorker :: Socket -> Chan (Packet, SockAddr) -> IO ()
      sendWorker sock tx = do
        forkIO $ forever $ do
          (pkt, addr) <- readChan tx
          SB.sendTo sock (encodePacket pkt) addr
          return ()
        return ()

      newContextInfo :: IO ContextInfo
      newContextInfo = ContextInfo <$> newMVar M.empty <*> newMVar [] <*> newMVar M.empty <*> newChan

      newSocket :: String -> PortNumber -> IO (Socket, SockAddr)
      newSocket host port = do
        -- TODO: suppport IPv6
        sock <- socket AF_INET Datagram defaultProtocol
        let addr = (SockAddrInet port iNADDR_ANY)
        bind sock addr
        return (sock, addr)


closeManager :: Manager -> IO ()
closeManager mgr = close (managerSocket mgr)

newConnectionId :: ContextInfo -> IO ConnectionId
newConnectionId info = do
    list <- readMVar ( contextInfoConnectionIds info)
    return $ getUnusedId list
    where
      getUnusedId [] = 1
      getUnusedId (x:xs) = getUnusedId' x xs
        where
          getUnusedId' x [] = x+1
          getUnusedId' x (y:ys)
            | (x > 2^16) = 2^16
            | (x + 1 == y) = getUnusedId' y ys
