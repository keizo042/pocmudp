module Network.MUDP.Session
  (
    newSession
  , recvDataAll
  , sendDataAll
  )
  where

import Network.MUDP.Types

import Control.Concurrent
import Control.Concurrent.MVar

import Data.List (sortBy)
import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BSC

newSession :: StreamId ->  IO Session
newSession sid = do
    tx <- newChan
    rx <- newChan
    fin <- newMVar False
    fsize <- newMVar 1024
    return $ Session sid tx rx fin fsize


sendDataAll :: Session -> ByteString -> IO ()
sendDataAll sess bs = do
    i <- readMVar (sessionFrameSize sess)
    send i bs 0
    where
      send i bss n = if BSC.null bss
        then return ()
        else do
          let (b, bs) = BSC.splitAt i bss
          writeChan (sessionContextTx sess) (sessionStreamId sess, bss, n)
          send i bs $ n + (fromIntegral $ BSC.length b)

recvDataAll :: Session   -> IO ByteString
recvDataAll sess = do
    takeMVar (sessionFin sess)
    -- TODO: melt  concat, sort and validate in a procedure
    (BSC.concat . map fst . sortBy cmp) <$> getChanContents (sessionContextRx sess)
    where
      cmp l r = compare (snd l) (snd r)

