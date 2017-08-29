{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.POC
import qualified Data.ByteString.Char8 as BS8

main :: IO ()
main = do
    mgr <- newManager "localhost" 8081
    ctx <- open mgr   "localhost" 8080 -- Opening Connection
    send ctx "hello world" -- Send Data as Stream Frame
    bs <- recv ctx -- recieve Stream Frame
    return ()
{-

    BS8.putStrLn  bs
    close ctx -- send Connection Close
    closeManager mgr
    -}
