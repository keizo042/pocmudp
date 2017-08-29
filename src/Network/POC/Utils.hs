module Network.POC.Utils
  (
  createContext
  , selectID
  )where


import Network.POC.Types

import Network.Socket

import Control.Concurrent
import Data.Map
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import qualified Data.List as L

createContext :: Manager -> SockAddr -> ConnectionId -> IO Context
createContext mgr addr c = do
  c' <- newMVar c
  tx <- newChan
  rx <- newChan
  fromConn <- newChan
  fin <- newEmptyMVar
  stream <- newChan
  streamid <- newMVar empty
  closed <- newEmptyMVar
  peer <- newMVar addr
  return $ Context tx rx fromConn c' fin stream streamid closed peer

selectID :: Manager -> IO ConnectionId
selectID mgr = do
  ids <- takeMVar $ managerConnectionIds mgr
  i <- choiceConnectionId 1 ids
  putMVar (managerConnectionIds mgr) (L.insert i ids)
  return i
   where
    choiceConnectionId :: ConnectionId -> [ConnectionId] -> IO ConnectionId
    choiceConnectionId _ [] = return 1
    choiceConnectionId i (x:xs)
        | (i == x) =  choiceConnectionId (i+1) (x:xs)
        | (i < x) = return i
        | (i > x) = choiceConnectionId i xs

