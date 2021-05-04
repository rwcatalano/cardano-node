{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Fee calculation
--
module Cardano.Api.Fees (
    transactionFee,
    estimateTransactionFee,
  ) where

import           Prelude

import qualified Data.ByteString as BS
import           GHC.Records (HasField (..))
import           Numeric.Natural

import qualified Cardano.Binary as CBOR
import qualified Cardano.Chain.Common as Byron

--import qualified Cardano.Ledger.Core as Core
import qualified Shelley.Spec.Ledger.LedgerState as Shelley
import qualified Shelley.Spec.Ledger.Tx as Shelley

import           Cardano.Api.Eras
import           Cardano.Api.NetworkId
import           Cardano.Api.Tx
import           Cardano.Api.Value


-- ----------------------------------------------------------------------------
-- Transaction fees
--

-- | For a concrete fully-constructed transaction, determine the minimum fee
-- that it needs to pay.
--
-- This function is simple, but if you are doing input selection then you
-- probably want to consider estimateTransactionFee.
--
transactionFee :: ShelleyBasedEra era
               -> Natural -- ^ The fixed tx fee
               -> Natural -- ^ The tx fee per byte
               -> Tx era
               -> Lovelace
transactionFee sbe txFeeFixed txFeePerByte tx =
   getFee
  where
    getFee :: Lovelace
    getFee =
      case tx of
        ShelleyTx _ tx' -> let x = getTxSize sbe tx'
                           in Lovelace (a * x + b)
        ByronTx _ -> case sbe :: ShelleyBasedEra ByronEra of {}

    getTxSize :: ShelleyBasedEra era -> Shelley.Tx (ShelleyLedgerEra era) -> Integer
    getTxSize ShelleyBasedEraShelley = Shelley.txsize
    getTxSize ShelleyBasedEraAllegra = Shelley.txsize
    getTxSize ShelleyBasedEraMary = Shelley.txsize
 -- TODO: Change Shelley.Tx to Tx type family
 -- getTxSize ShelleyBasedEraAlonzo = getField @"txsize"

    a = toInteger txFeePerByte
    x = getField @"txsize" tx
    b = toInteger txFeeFixed

--TODO: in the Byron case the per-byte is non-integral, would need different
-- parameters. e.g. a new data type for fee params, Byron vs Shelley

-- | This can estimate what the transaction fee will be, based on a starting
-- base transaction, plus the numbers of the additional components of the
-- transaction that may be added.
--
-- So for example with wallet coin selection, the base transaction should
-- contain all the things not subject to coin selection (such as script inputs,
-- metadata, withdrawals, certs etc)
--
estimateTransactionFee :: ShelleyBasedEra era
                       -> NetworkId
                       -> Natural -- ^ The fixed tx fee
                       -> Natural -- ^ The tx fee per byte
                       -> Tx era
                       -> Int -- ^ The number of extra UTxO transaction inputs
                       -> Int -- ^ The number of extra transaction outputs
                       -> Int -- ^ The number of extra Shelley key witnesses
                       -> Int -- ^ The number of extra Byron key witnesses
                       -> Lovelace
estimateTransactionFee sbe nw txFeeFixed txFeePerByte (ShelleyTx era tx) =
    let Lovelace baseFee = transactionFee sbe txFeeFixed txFeePerByte (ShelleyTx era tx)
    in \nInputs nOutputs nShelleyKeyWitnesses nByronKeyWitnesses ->

      --TODO: this is fragile. Move something like this to the ledger and
      -- make it robust, based on the txsize calculation.
      let extraBytes :: Int
          extraBytes = nInputs               * sizeInput
                     + nOutputs              * sizeOutput
                     + nByronKeyWitnesses    * sizeByronKeyWitnesses
                     + nShelleyKeyWitnesses  * sizeShelleyKeyWitnesses

      in Lovelace (baseFee + toInteger txFeePerByte * toInteger extraBytes)
  where
    sizeInput               = smallArray + uint + hashObj
    sizeOutput              = smallArray + uint + address
    sizeByronKeyWitnesses   = smallArray + keyObj + sigObj + ccodeObj + attrsObj
    sizeShelleyKeyWitnesses = smallArray + keyObj + sigObj

    smallArray  = 1
    uint        = 5

    hashObj     = 2 + hashLen
    hashLen     = 32

    keyObj      = 2 + keyLen
    keyLen      = 32

    sigObj      = 2 + sigLen
    sigLen      = 64

    ccodeObj    = 2 + ccodeLen
    ccodeLen    = 32

    address     = 2 + addrHeader + 2 * addrHashLen
    addrHeader  = 1
    addrHashLen = 28

    attrsObj    = 2 + BS.length attributes
    attributes  = CBOR.serialize' $
                    Byron.mkAttributes Byron.AddrAttributes {
                      Byron.aaVKDerivationPath = Nothing,
                      Byron.aaNetworkMagic     = toByronNetworkMagic nw
                    }

