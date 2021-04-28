import           Data.Version (showVersion)
import           Options.Applicative (ParserInfo, (<**>), customExecParser, fullDesc, header,
                                      help, helper, info, infoOption, long, prefs, short,
                                      showHelpOnEmpty)

import           Cardano.Logger.CLI (LoggerParams, parseLoggerParams)
import           Cardano.Logger.Run (runCardanoLogger)
import           Paths_cardano_logger (version)

main :: IO ()
main = do
  loggerParams <- customExecParser (prefs showHelpOnEmpty) loggerInfo
  runCardanoLogger loggerParams
 where
  loggerInfo :: ParserInfo LoggerParams
  loggerInfo = info
    (parseLoggerParams <**> helper <**> versionOption)
    (fullDesc <> header "cardano-logger - the logging service for cardano node.")
  versionOption = infoOption
    (showVersion version)
    (long "version" <>
     short 'v' <>
     help "Show version")
