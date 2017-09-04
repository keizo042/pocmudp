{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad(when)
import Network.MUDP
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS8

main :: IO ()
main =  client `E.catch` handle

client :: IO ()
client = do
    putStrLn "init manger"
    mgr <- newManager "localhost" 8082
    putStrLn "connect to remote host"
    ctx <- connect mgr   "localhost" "8081" -- Opening Connection , when it fail, throw SomeException
    putStrLn "send msg"
    b <- send ctx "hello world" -- Send Data as Stream Frame
    when b $ do
      putStrLn "recv massage"
      bs <- recv ctx -- recieve Stream Frame
      BS8.putStrLn  bs
    close ctx -- send Connection Close
    closeManager mgr
    putStrLn "terminate"


handle :: E.SomeException -> IO ()
handle _ = return ()
