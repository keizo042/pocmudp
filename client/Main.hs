{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.MUDP
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS8

main :: IO ()
main =  client `E.catch` handle

client :: IO ()
client = do
    mgr <- newManager "localhost" "8081"
    ctx <- connect mgr   "localhost" "8080" -- Opening Connection , when it fail, throw SomeException
    send ctx "hello world" -- Send Data as Stream Frame
    bs <- recv ctx -- recieve Stream Frame
    BS8.putStrLn  bs
    close ctx -- send Connection Close
    closeManager mgr


handle :: E.SomeException -> IO ()
handle _ = return ()
