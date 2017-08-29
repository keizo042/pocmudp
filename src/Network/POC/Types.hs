module Network.POC.Types where

import Data.Int
import Data.Map
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad

import Network.Socket



type ConnectionId = Int16

type StreamId = Int16

type Offset = Int16

data Header = Header { headerConnectionId :: !ConnectionId }
              deriving (Eq, Show)

data Frame = Stream !Bool !StreamId !BS.ByteString
          |  ConnectionClose
          |  Initial ConnectionId
          deriving (Eq, Show)
-- |
-- Manager is that manager all context in a endpoin of QUIC.
data Manager = Manager { managerSocket :: Socket
                        ,managerTx :: Chan (ByteString, SockAddr)
                        ,managerRx :: Chan (ByteString, SockAddr)
                        ,managerHandlerId :: MVar ThreadId
                        ,managerDeliverId :: MVar ThreadId
                        ,managerConnectionIds :: MVar [ConnectionId]
                        ,managerNewComer :: Chan (ConnectionId, SockAddr)
                        ,managerConnectionRxs :: MVar (Map ConnectionId (Chan ByteString))
                        ,managerToContext :: MVar (Map ConnectionId Context) }

-- |
-- Context is that has infomation related 4 tuple
-- on the other word, It is called `Session`
data Context = Context { contextTx :: Chan ByteString
                        , contextRx :: Chan ByteString
                        , contextConnectionId :: MVar ConnectionId
                        , contextFin :: MVar Bool
                        , contextClosed :: MVar Bool
                        , contextRemoteHost :: MVar SockAddr}

