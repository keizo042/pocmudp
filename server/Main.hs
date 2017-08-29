{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad(when)
import Control.Concurrent
import Control.Concurrent.MVar
import Network.POC
import Data.ByteString.Char8

main :: IO ()
main = do
    mgr <- newManager "localhost" 8080
    server mgr


server :: Manager ->  IO ()
server mgr = dispatch mgr 1024


dispatch :: Manager -> Int -> IO ()
dispatch mgr limit = do
    counter <- newMVar 0
    dispatch' mgr counter
    where
      dispatch' :: Manager -> MVar Int -> IO ()
      dispatch' mgr cnt = do
        i <- takeMVar cnt
        if i <= limit
          then do
            ctx <- accept mgr
            putMVar cnt (i+1)
            dispatch mgr limit
          else do
            putMVar cnt i
            dispatch mgr limit

worker :: Context -> (MVar Int) -> IO ()
worker ctx ctr = do
  forkIO $ do
    bs <- recv ctx
    send ctx $ pack ("pong to " ++ "\""  ++  (unpack bs) ++ "\"")
  wait ctx ctr
    where
      wait ctx ctr = do
        b <- contextIsClosed ctx
        when b $ modifyMVar ctr (\i -> return (i - 1, ()))
        wait ctx ctr
