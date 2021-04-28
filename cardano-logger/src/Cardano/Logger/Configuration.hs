{-# LANGUAGE LambdaCase #-}

module Cardano.Logger.Configuration
  ( readLoggerConfig
  , getAcceptors
  ) where

import           Control.Exception (IOException, catch)
import qualified System.Exit as Ex

import           Cardano.BM.Configuration (Configuration, getAcceptAt, setup)
import           Cardano.BM.Data.Configuration (RemoteAddrNamed (..))

-- | Reads the program's configuration file
--   (path is passed via '--config' CLI option).
readLoggerConfig :: FilePath -> IO Configuration
readLoggerConfig pathToConfig = setup pathToConfig `catch` problems
 where
  problems :: IOException -> IO Configuration
  problems e = Ex.die $
    "Exception while reading configuration '" <> pathToConfig <> "': " <> show e

-- | Logger requires at least one acceptor point
--   (it will be used to accept log items from the node).
getAcceptors :: Configuration -> IO [RemoteAddrNamed]
getAcceptors config =
  getAcceptAt config >>= \case
    Just acceptors -> return acceptors
    Nothing -> Ex.die "No acceptors found in the configuration, please add at least one."
