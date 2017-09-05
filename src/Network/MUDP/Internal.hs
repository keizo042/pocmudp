module Network.MUDP.Internal
  (
    send
  , recv
  , close
  , connect
  , listen
  )
  where

import Data.ByteString (ByteString)
import qualified Network.Socket as S
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar

import Network.MUDP.Types
import Network.MUDP.Codec
import Network.MUDP.Manager
import Network.MUDP.Session
import Network.MUDP.Context

-- Manager Level operation
-- and init/terminate context

connect :: Manager -> String -> String -> IO Context
connect mgr host port = do
    info <- head <$> S.getAddrInfo (Just hints) (Just host) (Just port)
    let pkt = Packet (Header Handshake Nothing) [ClientInitial]
        addr = S.addrAddress info
    ctx <- newContext Nothing addr
    writeChan (managerTx mgr) (pkt, addr)
    -- TODO: timeout operation when it can not recv handshake repsonse
    readMVar (contextConnectionId ctx)
    return ctx
    where
      hints = S.defaultHints { S.addrSocketType = S.Datagram }

-- TODO: timeout operation
listen :: Manager -> IO Context
listen mgr = readChan (contextInfoNextOne $ managerContextInfo mgr)


-- Context level operation

send :: Context -> ByteString -> IO Bool
send ctx bs = do
    i <- takeMVar (contextNextStreamId ctx)
    sess <- newSession i
    putMVar (contextNextStreamId ctx) (i + 1)
    sendDataAll sess bs
    writeChan (contextNextSession ctx) sess
    return True

recv :: Context -> IO ByteString
recv ctx = do
    sess <- readChan (contextNextSession ctx)
    recvDataAll sess

close :: Context -> IO Bool
close ctx = do
    i <- readMVar (contextConnectionId ctx)
    send ctx $ encodePacket $ pkt i
  where
    pkt :: ConnectionId -> Packet
    pkt i = Packet (Header Transport (Just i)) [ConnectionClose]
