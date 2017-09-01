module Network.MUDP.Codec
  (
    decodePacket
  , encodePacket
  )
  where

import Network.MUDP.Types

import Data.Bits
import Data.Int
import Data.Serialize
import Data.Serialize.Get
import Data.Serialize.Put

import qualified Data.ByteString.Char8 as BSC

decodePacket = undefined

encodePacket = undefined

getConnectionId :: Get ConnectionId
getConnectionId = fromIntegral <$> getInt16be

getHeader :: Get Header
getHeader = getType >>= getHdr
  where
    getType = fromIntegral <$> getInt8
    getHdr i = do
      c <- if  (i .&. 0x40 == 0x40)  then Just <$> getConnectionId else return Nothing
      return $ Header (toConnectionState i) c
      where
        toConnectionState :: Int -> ConnectionState
        toConnectionState i = if (i .&. 0x80 == 0x80) then Handshake else Transport


getFrame :: Get Header
getFrame = getTyp >>= getFrm
  where
    getTyp = fromIntegral <$> getInt8
    getFrm i = undefined


fromConnectionState :: ConnectionState -> Int
fromConnectionState Handshake = 0x80
fromConnectionState Transport = 0x00

putConnectionId :: Putter ConnectionId
putConnectionId i = putInt16be $ fromIntegral i

putHeader :: Putter Header
putHeader (Header s c)    = do
    putInt8 . fromIntegral $  fromConnectionState s .|. hasConnectionId c
    putMaybeConnectionId c
    where
      putMaybeConnectionId :: Putter (Maybe ConnectionId)
      putMaybeConnectionId Nothing = return ()
      putMaybeConnectionId (Just c) = putConnectionId c

      hasConnectionId Nothing = 0x00
      hasConnectionId (Just _) = 0x40


putFrame :: Putter Frame
putFrame (Stream fin i o bs) = do
    putInt8 (0x80 .|. finTob fin)
    putStreamId i
    putOffset o
    putData bs
    where
      finTob :: Bool -> Int8
      finTob True = 0x01
      finTob False = 0x00
      putStreamId = putInt16be . fromIntegral
      putOffset = putInt16be . fromIntegral
      putData bs = do
        putInt16be . fromIntegral $ BSC.length bs
        putByteString bs

putFrame ConnectionClose = do
    putInt8 0x40
putFrame ClientInitial = do
    putInt8 0x20
putFrame (ServerResponse c)  = do
    putInt8 0x10
    putConnectionId c
