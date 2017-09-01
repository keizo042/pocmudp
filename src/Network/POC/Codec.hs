module Network.POC.Codec
  (
    decodeHeader
  , encodeHeader
  , decodeFrames
  , decodeFrame
  , encodeFrame
  )where

import Network.POC.Types

import Data.Serialize
import Data.Serialize.Get
import Data.Serialize.Put
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Int
import Data.Bits

--    0                   1                   2                   3
--    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |	    Type     | Connection Id (16)                |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |                          Frames  (*)                        ...
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--			Figure 1. Header Format
--
-- - Connection Id is that identfy connection with 4 tuple (local ip, local port, remote ip, remote port)
--


--    0                   1                   2                   3
--    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |1              | Connectino Id                     |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--			Figure 3. Connection Close Packet


--    0                   1                   2                   3
--    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |1|           |f| 		Stream Id(16)   	 |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   | Offset(16)                    |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   | data length(8)| data (*)
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

--			Figure 2. Stream Frame
--
-- - header ( 0xf0 in first octet) is that indicate finish stream.
-- - Packet Type is that mean what kind packet is.
-- - data length is that indicate length of data field



--    0                   1                   2                   3
--    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |001            |      Connection Id                |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--			Figure 3. Initial Frame


getHeader  :: Get Header
getHeader = Header <$> getInt16be

putHeader :: Header -> Put
putHeader (Header c) =  putInt16be c

getFrame :: Get Frame
getFrame = getType >>= get
  where
    get i
      | (i .&. 0x80 == 0x80) = getStream i
      | (i .&. 0x40 == 0x40) = getConnectionClose i
      | (i .&. 0x20 == 0x20) = getInitial i

    getType :: Get Int
    getType = fromIntegral <$> getInt8

    getStream :: Int -> Get Frame
    getStream i = Stream (isFin i) <$> getStreamId <*> getOffset <*> getData
      where
        isFin :: Int -> Bool
        isFin i = i .&. 0x01 == 0x01
        getStreamId = getInt16be
        getOffset = getInt16be
        getData = getInt8 >>= getBytes . fromIntegral
    getConnectionClose :: Int -> Get Frame
    getConnectionClose i = ConnectionClose <$> getInt16be
    getInitial :: Int -> Get Frame
    getInitial i = Initial <$> getInt16be

putFrame :: Frame -> Put
putFrame (f@Stream{}) = putStream f
putFrame (f@ConnectionClose{}) = putConnectionClose f
putFrame (f@Initial{}) = putInitial f

putStream (Stream fin sid offset bs) = do
  putInt8 0x80
  putInt16be sid
  putInt16be offset
  putInt8 $ fromIntegral $ BS.length bs
  putByteString bs

putConnectionClose (ConnectionClose c) = do
  putInt8 0x40
  putInt16be c

putInitial (Initial i) = do
  putInt8 0x20
  putInt16be i

decodeHeader :: BS.ByteString -> Maybe (Header, BS.ByteString)
decodeHeader = undefined

encodeHeader :: Header -> BS.ByteString
encodeHeader = undefined

decodeFrame :: BS.ByteString -> (Maybe Frame, BS.ByteString)
decodeFrame = undefined

decodeFrames :: BS.ByteString -> [Maybe Frame]
decodeFrames bs = if BS.null bs
                    then []
                    else  let (f, bs') = decodeFrame bs in f : decodeFrames bs'

encodeFrame :: Frame -> BS.ByteString
encodeFrame = runPut . putFrame

