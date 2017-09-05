module Network.MUDP.Types
  (
    Manager(..)
  , ContextInfo(..)
  , Context(..)
  , Session(..)

  , ConnectionId
  , StreamId
  , Offset

  ,Packet(..)
  ,Header(..)
  ,ConnectionState(..)
  ,Payload(..)
  ,Frame(..)
  )
  where

import Data.Int
import Network.Socket
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Data.ByteString (ByteString)
import qualified Data.Map as M

type ConnectionId = Int16

type StreamId = Int16

type Offset = Int16

data ConnectionState = Handshake  -- 0x80
                     | Transport  -- 0x00
                     deriving (Eq, Show)

data Packet = Packet Header Payload

data Header = Header ConnectionState (Maybe ConnectionId) -- 0x40 in type field indicate wheter present  or not
            deriving (Eq, Show)

type Payload = [Frame]

data Frame = Stream Bool StreamId Offset ByteString
           -- Special Frame, they are only one in the packet
           | ConnectionClose
           | ClientInitial
           | ServerResponse ConnectionId
           deriving (Eq, Show)

data ContextInfo = ContextInfo { contextInfoConnections :: MVar (M.Map ConnectionId Context)
                                ,contextInfoConnectionIds ::  MVar [ConnectionId]
                                ,contextInfoAddrToConnection :: MVar (M.Map SockAddr Context)
                                ,contextInfoNextOne :: Chan Context
                                }

data Manager = Manager  { managerSocket :: Socket
                        , managerLocalSockAddr :: SockAddr
                        , managerContextInfo :: ContextInfo
                        , managerTx :: Chan (Packet, SockAddr)
                        , managerRx :: Chan (ByteString, SockAddr)
                        }

data Context = Context { contextConnectionId :: MVar ConnectionId
                        ,contextRemoteSockAddr :: MVar SockAddr
                        ,contextNextStreamId :: MVar StreamId
                        ,contextStreamIds :: MVar [StreamId]
                        ,contextTx :: Chan (Packet, SockAddr)
                        ,contextRx :: Chan (ByteString, SockAddr)
                        ,contextClosed :: MVar Bool
                        ,contextSessions :: MVar (M.Map StreamId Session)
                        ,contextDiscoardingSession :: Chan StreamId
                        ,contextNextSession :: Chan Session
                        }

data Session = Session { sessionStreamId :: StreamId
                        ,sessionContextTx :: Chan (StreamId, ByteString, Offset)
                        ,sessionContextRx :: Chan (ByteString, Offset)
                        ,sessionFin :: MVar Bool
                        ,sessionFrameSize :: MVar Int
                        }
