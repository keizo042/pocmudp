{-# LANGUAGE OverloadedStrings #-}
module CodecSpec (spec) where


import Network.MUDP.Codec
import Network.MUDP.Types
import Data.Serialize


import Test.Hspec

codec :: (Get a) -> (Putter a) -> a -> Either String a
codec g p d = (runGet g) $ runPut (p d)

hdr1 = Header Handshake Nothing
hdr2 = Header Transport (Just 0x42)
frame1 = Stream False 0x42 0 "hello"
frame2 = ConnectionClose
frame3 = ClientInitial
frame4 = ServerResponse

spec :: Spec
spec = do
    describe "codec" $ do
      it "decode Header in handshake" $
        (codec getHeader putHeader hdr1) `shouldBe` (Right hdr1)
      it "decode Header in transport" $
        (codec getHeader putHeader hdr2) `shouldBe` (Right hdr2)
      it "decode Frame as Strame" $
        (codec getFrame putFrame frame1) `shouldBe` (Right frame1)
      it "decode Frame as ConnectionClose" $
        (codec getFrame putFrame frame2) `shouldBe` (Right frame2)
      it "decode Frame as ClientInitial" $
        (codec getFrame putFrame frame3) `shouldBe` (Right frame3)
      it "decode Frame as ServerResponse" $
        (codec getFrame putFrame frame4) `shouldBe` (Right frame4)
