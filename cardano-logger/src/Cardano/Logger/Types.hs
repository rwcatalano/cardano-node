module Cardano.Logger.Types
  ( AcceptedItems
  , LogObjects
  , Metrics
  , NodeId
  , initAcceptedItems
  , prepareAcceptedItems
  ) where

import           Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO)
import           Control.Monad (unless)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import           Data.Text (Text)
import qualified System.Metrics as EKG

import           Cardano.BM.Data.LogItem (LogObject)

import           System.Metrics.Store.Acceptor (MetricsLocalStore, emptyMetricsLocalStore)

type NodeId = String

type LogObjects = TBQueue (LogObject Text)
type Metrics    = (EKG.Store, IORef MetricsLocalStore)

type AcceptedItems = IORef (HashMap NodeId (LogObjects, Metrics))

initAcceptedItems :: IO AcceptedItems
initAcceptedItems = newIORef $ HM.empty

prepareAcceptedItems
  :: String
  -> AcceptedItems
  -> IO ()
prepareAcceptedItems nodeId itemsIORef = do
  items' <- readIORef itemsIORef
  -- If such 'nodeId' is already presented in 'items', it means that this node
  -- already worked with the logger and now it's re-connect to the logger.
  -- No need to re-create its stores.
  unless (nodeId `HM.member` items') $ do
    loQueue <- newTBQueueIO 2000
    ekgStore <- EKG.newStore
    localStore <- newIORef emptyMetricsLocalStore
    let storesForNewNode = (loQueue, (ekgStore, localStore))
    atomicModifyIORef' itemsIORef $ \items ->
      (HM.insert nodeId storesForNewNode items, ())
