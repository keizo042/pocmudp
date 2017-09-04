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


connect :: Manager -> String -> String -> IO Context
connect mgr host port = do
    addr <- head <$> S.getAddrInfo (Just hints) (Just host) (Just port)
    c <- newConnectionId mgr
    newContext c $ S.addrAddress addr
    where
      hints = S.defaultHints { S.addrSocketType = S.Datagram }

listen :: Manager -> IO Context
listen mgr = readChan (managerNextConnection mgr)

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
