
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Node.Orphans () where

import           Cardano.Prelude
import           Prelude (fail)

import           Cardano.Api.Orphans ()

import           Data.Aeson (FromJSON (..), ToJSON (..), ToJSONKey, Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Short as SBS
import qualified Data.Text as Text



import           Cardano.BM.Data.Tracer (TracingVerbosity (..))
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Ledger.Alonzo as Alonzo
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Compactible as Ledger
import qualified Cardano.Ledger.Mary.Value as Mary
import qualified Data.MemoBytes as MemoBytes
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)
import qualified Shelley.Spec.Ledger.CompactAddr as Shelley

instance FromJSON TracingVerbosity where
  parseJSON (String str) = case str of
    "MinimalVerbosity" -> pure MinimalVerbosity
    "MaximalVerbosity" -> pure MaximalVerbosity
    "NormalVerbosity" -> pure NormalVerbosity
    err -> fail $ "Parsing of TracingVerbosity failed, "
                <> Text.unpack err <> " is not a valid TracingVerbosity"
  parseJSON invalid  = fail $ "Parsing of TracingVerbosity failed due to type mismatch. "
                           <> "Encountered: " <> show invalid

deriving instance Show TracingVerbosity

deriving instance ToJSON (Alonzo.PParamsUpdate (Alonzo.AlonzoEra StandardCrypto))
deriving instance ToJSON Alonzo.ExUnits
deriving instance ToJSON Alonzo.Prices
deriving instance ToJSON Alonzo.Language
deriving instance ToJSON (MemoBytes.MemoBytes (Map ByteString Integer))

-- Plutus scripts are encoded as ShortByteString.
--TODO: I assume we are not interested in rendering
--plutus scripts as JSON.
instance ToJSON SBS.ShortByteString where
  toJSON _plutusScriptBytes = Aeson.String "Plutus script placeholder"

-- Obviously incorrect. Need to fix.
deriving instance ToJSONKey SBS.ShortByteString
-- Obviously incorrect. Need to fix.
deriving instance ToJSONKey ByteString
deriving instance ToJSONKey Alonzo.Language
deriving instance ToJSON Alonzo.CostModel


deriving instance ToJSON (Alonzo.TxOut (Alonzo.AlonzoEra StandardCrypto))

deriving instance ToJSON (Shelley.CompactAddr StandardCrypto)
deriving instance Generic (Shelley.CompactAddr StandardCrypto)

instance ToJSON (Ledger.CompactForm (Mary.Value StandardCrypto)) where
  toJSON valCompForm = Aeson.String . Text.pack $ show valCompForm

instance FromJSON Update.ApplicationName where
  parseJSON (String x) = pure $ Update.ApplicationName x
  parseJSON invalid  =
    fail $ "Parsing of application name failed due to type mismatch. "
    <> "Encountered: " <> show invalid
