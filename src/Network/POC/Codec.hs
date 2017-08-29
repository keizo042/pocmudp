module Network.POC.Codec
  (
    decodeHeader
  , encodeHeader
  , decodeFrames
  , decodeFrame
  , encodeFrame
  )where

import Network.POC.Types

import qualified Data.Serialize as SZ
import qualified Data.ByteString as BS

--    0                   1                   2                   3
--    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |	    Connection Id            |
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
--   |1|           |f| 		Stream Id       	 |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   | Offset			     |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   | data length   | data (*)
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

--			Figure 2. Stream Frame
--
-- - header ( 0xf0 in first octet) is that indicate finish stream.
-- - Packet Type is that mean what kind packet is.
-- - data length is that indicate length of data field

--    0                   1                   2                   3
--    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |01             |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--			Figure 3. Connection Close Frame


--    0                   1                   2                   3
--    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |001            |      Connection Id                |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--			Figure 3. Initial Frame


decodeHeader :: BS.ByteString -> (Header, BS.ByteString)
decodeHeader = undefined

encodeHeader :: Header -> BS.ByteString
encodeHeader = undefined

decodeFrame :: BS.ByteString -> (Frame, BS.ByteString)
decodeFrame = undefined

decodeFrames :: BS.ByteString -> [Frame]
decodeFrames bs = if BS.null bs
                    then []
                    else  let (f, bs') = decodeFrame bs in f : decodeFrames bs'

encodeFrame :: Frame -> BS.ByteString
encodeFrame = undefined


