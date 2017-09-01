module Network.POC.Manager
  (
   newManager
  ,closeManager
  )
  where

import Network.POC.Types
import Network.POC.Utils
import Network.POC.Codec

import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad

import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SB

import Data.Int
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe


newManager :: String -> Int -> IO Manager
newManager host port = do
  mgr <- new host port
  handler <- handle (managerSocket mgr) (managerRx mgr) (managerTx mgr)
  deliver <- forkIO $ forever $ delive mgr
  putMVar (managerHandlerId mgr) handler
  putMVar (managerDeliverId mgr) deliver
  return mgr

  where
    handle :: S.Socket
            -> Chan (ByteString, S.SockAddr)
            -> Chan (ByteString, S.SockAddr)
            -> IO ThreadId
    handle sock tx rx = forkIO $ do
      forkIO $ sending  sock tx
      forkIO $ reciveing sock rx 1024
      return ()
      where
        sending :: S.Socket -> Chan (ByteString, S.SockAddr) -> IO ()
        sending sock tx = do
          (bs, addr) <- readChan tx
          i <- SB.sendTo sock bs addr
          sending sock tx

        reciveing :: S.Socket -> Chan (ByteString, S.SockAddr) -> Int -> IO ()
        reciveing sock rx size = do
          r <- SB.recvFrom sock size
          writeChan rx r
          reciveing sock rx size

    new host port = do
      sock <- socket' host port
      tx <- newChan
      rx <- newChan
      handler <- newEmptyMVar
      deliver <- newEmptyMVar
      newcomer <- newChan
      conns <- newMVar []
      connRxs <- newMVar M.empty
      toCtx <- newMVar (M.empty :: M.Map ConnectionId Context)
      return $ Manager sock tx rx handler deliver conns newcomer connRxs toCtx
      where
        socket' host port = do
          let hints = S.defaultHints { S.addrSocketType = S.Datagram, S.addrProtocol = 17 }
          info <- head <$> S.getAddrInfo  (Just hints)
                                          (Just (host))
                                          (Just (show port))
          sock <- S.socket (S.addrFamily info) S.Datagram S.defaultProtocol
          return sock




    delive :: Manager -> IO ()
    delive mgr = do
      (bs, addr)  <- readChan (managerRx mgr)
      delive' mgr bs addr (managerToContext mgr)
      where
        delive' :: Manager -> ByteString -> S.SockAddr -> (MVar (M.Map ConnectionId Context)) -> IO ()
        delive' mgr bs addr mm = do
          m <- readMVar mm
          let c = headerConnectionId (fst $ decodeHeader bs)
          if isNothing c
            then return ()
            else case (M.lookup (fromJust c) m) of
            (Just ctx') -> do
              writeChan (contextRx ctx') bs
            Nothing  -> do
              i <- selectID mgr
              ctx <- createContext mgr addr i
              return ()



closeManager :: Manager -> IO ()
closeManager mgr =  do
    S.close $ managerSocket mgr
    return ()
