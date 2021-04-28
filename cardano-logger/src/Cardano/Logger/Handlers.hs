module Cardano.Logger.Handlers
  ( runHandlers
  ) where

import           Cardano.BM.Configuration (Configuration)

import           Cardano.Logger.Types (AcceptedItems)

runHandlers
  :: Configuration
  -> AcceptedItems
  -> IO ()
runHandlers _config _acceptedItems = do
  return ()
