module Network.MUDP.Types
  (
    Context(..)
  , Session(..)
  , SessionInfo(..)
  , Packet(..)
  , Header(..)
  , Frame(..)

  , Phase(..)
  , ConnectionId
  , StreamId
  , Offset
  )
  where

import Data.Int
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import Data.Map(Map)
import qualified Data.Map as M

import Data.Sequence 
-- using Seq as synchronize Chan
-- TODO: make it async-safe

import Network.Socket


data Context = Context { contextConnectionId :: ConnectionId
                       , contextSockAddr :: SockAddr
                       , contextTxSeq :: Seq (ByteString, SockAddr)
                       , contextRxSeq :: Seq ByteString
                       , contextClosed :: Bool
                       , contextSessionInfo :: SessionInfo
                        }

data SessionInfo = SessionInfo { sessionInfoSessions :: Map StreamId Session
                                ,sessionInfoDiscarding :: Seq StreamId
                                ,sessionInfoNext :: Seq StreamId
                                }



data Session = Session {  sessionNextOne :: Seq StreamId
                        , sessionTx :: Seq (ByteString, Offset)
                        , sessionRx :: Seq (ByteString, Offset)
                        }


data Packet = Packet Header [Frame]

data Header = Header Phase (Maybe ConnectionId)
            deriving (Eq, Show)

data Phase = Handhsake | Transport
           deriving (Eq, Show)

type ConnectionId = Int64

type Offset = Int32

type StreamId = Int32

data Frame = Stream StreamId Bool Offset ByteString
           | ClientInitial
           | ServerHello
           deriving Show

