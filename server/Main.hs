{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.MUDP
import qualified Data.ByteString.Char8 as BSC

main :: IO ()
main = do
    mgr <- newManager "localhost" 8080
    server mgr

server :: Manager ->  IO ()
server mgr = run mgr
  where
    run mgr = do
      ctx <- listen mgr
      handle ctx
      run mgr
      where
        handle ctx = do
          bs <- recv ctx
          send ctx $ (BSC.pack "hello world: ") `BSC.append` bs
          return ()

