{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Cardano.Logger.CLI
  ( LoggerParams (..)
  , parseLoggerParams
  ) where

import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics (Generic)
import           Options.Applicative (Parser, bashCompleter, completer,
                                      help, long, metavar, strOption)

-- | Type for CLI parameters required for the service.
data LoggerParams = LoggerParams
  { loggerConfig :: !FilePath
  } deriving (Generic, FromJSON, ToJSON)

parseLoggerParams :: Parser LoggerParams
parseLoggerParams =
  LoggerParams
    <$> parseFilePath
          "config"
          "file"
          "Configuration file for logger service"

-- Aux parsers

parseFilePath
  :: String
  -> String
  -> String
  -> Parser FilePath
parseFilePath optname completion desc = strOption flags
 where
  flags =
       long optname
    <> metavar "FILEPATH"
    <> help desc
    <> completer (bashCompleter completion)
