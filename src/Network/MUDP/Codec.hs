module Network.MUDP.Codec
  (

    decodeAll
  , encodeAll
  , decodeHeader
  , encodeHeader
  , decodeFrames
  , encodeFrames

  )
  where

import Data.Bits
import Data.Int
import Data.Serialize
import Data.Serialize.Put
import Data.Serialize.Get
import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BSC


import Network.MUDP.Types

getConnectionId :: Get ConnectionId
getConnectionId = fromIntegral <$> getInt64be

putConnectionId :: Putter ConnectionId
putConnectionId c = putInt64be $ fromIntegral c

getOffset :: Get Offset
getOffset = fromIntegral <$> getInt32be

putOffset :: Putter Offset
putOffset o = putInt32be $ fromIntegral o

getStreamId :: Get StreamId
getStreamId = fromIntegral <$> getInt32be

putStreamId :: Putter StreamId
putStreamId s = putInt32be $ fromIntegral s

getData :: Get ByteString
getData = getInt32be >>= (\ i -> getBytes $ fromIntegral i)

putData :: Putter ByteString
putData bs = do
    putInt32be $ fromIntegral $ BSC.length bs 
    putByteString bs

decodeAll :: ByteString -> Packet 
decodeAll bs = undefined

encodeAll :: Packet -> ByteString
encodeAll pkt = undefined

decodeHeader :: ByteString -> Maybe Header
decodeHeader bs = case (runGet getHeader bs) of
                    (Right hdr) -> Just hdr
                    _ -> Nothing

getHeader :: Get Header
getHeader = do
    w <- getWord8
    c <- getConnectionIdMaybe w
    return $ Header (isPhase w) c
    where
      isPhase w = if testBit w 7 
                    then Handhsake
                    else Transport
      getConnectionIdMaybe w = if testBit w 0
                                 then Just <$> getConnectionId
                                 else return Nothing


encodeHeader :: Header -> ByteString
encodeHeader hdr = runPut $ putHeader hdr

putHeader :: Putter Header
putHeader hdr = undefined

decodeFrames :: ByteString -> Maybe [Frame]
decodeFrames bs =  case (runGet getFrames bs) of
                     (Right fs) -> Just fs
                     _ -> Nothing

encodeFrames :: [Frame] -> ByteString
encodeFrames fs = runPut $ putFrames fs
getFrames :: Get [Frame]
getFrames = do
    b <- isEmpty 
    if b
      then return []
      else do
        f   <- getFrame
        fs  <- getFrames
        return $ f:fs


putFrames :: Putter [Frame]
putFrames [] =  return ()
putFrames (f:fs) = do
    putFrame f
    putFrames fs

getFrame :: Get Frame
getFrame = getWord8 >>= getFrame'
    where
      getFrame' w 
        | (testBit w 7) = do
            s <- getStreamId
            o <- getOffset
            bs <- getData 
            return $ Stream s (testBit w 0)  o bs 
        | (testBit w 6) = return ClientInitial
        | (testBit w 5) = return ServerHello




putFrame :: Putter Frame
putFrame (Stream sid fin o bs) = do
    putWord8 $ 0x80 .|. (if fin then 0x01 else 0x00)
    putStreamId sid
    putOffset o
    putData bs 
putFrame ClientInitial = undefined
putFrame ServerHello   = undefined
