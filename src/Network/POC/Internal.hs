module Network.POC.Internal where

import Network.POC.Codec
import Network.POC.Types
import Network.POC.Manager
import Network.POC.Utils

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




-- Client  <-----------------------> Server
-- send Initial
--                        recv Initial
--                        send AcK
-- recv AcK
-- send Stream
--                        recv Stream until getting one with Fin bit
--                         Figure 4: Handshake

-- |
-- contextIsClosed is that check that
-- whether the connection is closed or not
contextIsClosed :: Context -> IO Bool
contextIsClosed ctx =  fromMaybe True <$>  (tryReadMVar $ contextClosed ctx)


open :: Manager -> String -> Int -> IO Context
open mgr host port = do
  c <- selectID mgr
  s <- sock host port
  ctx <- createContext mgr s c
  registerContext mgr c ctx
    where
      sock host port = do
        let hints = S.defaultHints { S.addrSocketType = S.Datagram, S.addrProtocol = 17 }
        addr <- head <$> S.getAddrInfo (Just hints) (Just host) (Just $ show port)
        return $ S.addrAddress addr

      registerContext :: Manager -> ConnectionId -> Context -> IO Context
      registerContext mgr c ctx = modifyMVar (managerToContext mgr) (\ m -> return (M.insert c ctx m, ctx))



close :: Context -> IO ()
close ctx = do
    writeChan (contextTx ctx) $ encodeFrame ConnectionClose
    putMVar (contextClosed ctx) True
    return ()

accept :: Manager -> IO Context
accept mgr = do
    (c, sock) <- readChan (managerNewComer mgr)
    ctx <- createContext mgr sock c
    forkIO $ forever $  attachSockAddrToTx (managerTx mgr) (contextTx ctx) (contextRemoteHost ctx)
    withMVar (managerConnectionRxs mgr) (\ m -> return $ M.insert c (contextRx ctx) m)
    return ctx
    where
      attachSockAddrToTx :: Chan (ByteString, S.SockAddr) -> (Chan ByteString) -> (MVar S.SockAddr) -> IO ()
      attachSockAddrToTx mtx tx maddr = do
        addr <- readMVar maddr
        bs <- readChan tx
        writeChan mtx (bs,addr)
        return ()



send :: Context -> ByteString -> IO ()
send ctx  = writeChan (contextTx ctx)

recv :: Context -> IO ByteString
recv ctx = do
    takeMVar (contextFin ctx)
    bs <- getChanContents (contextRx ctx)
    return $ BS.concat bs

