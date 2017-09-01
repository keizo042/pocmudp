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
  ids <- readMVar $ managerConnectionIds mgr
  i <- choiceUnusedConnectionId 1 ids
  modifyMVar_ (managerConnectionIds mgr) (\is -> return $ L.insert i is)
  return i
   where
    choiceUnusedConnectionId :: ConnectionId -> [ConnectionId] -> IO ConnectionId
    choiceUnusedConnectionId _ [] = return 1
    choiceUnusedConnectionId i (x:xs)
        | (i == x) =  choiceUnusedConnectionId (i+1) (x:xs)
        | (i < x) = return i
        | (i > x) = choiceUnusedConnectionId i xs

