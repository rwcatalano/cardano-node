{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Cardano.Tracing.Constraints
  ( TraceConstraints
  ) where

import           Prelude (Show)

import           Data.Aeson

import           Cardano.BM.Tracing (ToObject)
import           Cardano.TraceDispatcher.Common.ConvertTxId (ConvertTxId')
import           Cardano.Tracing.ConvertTxId(ConvertTxId)
import           Cardano.Tracing.Queries (LedgerQueries)
import           Cardano.Logging (LogFormatting)


import           Ouroboros.Consensus.Block (BlockProtocol, CannotForge,
                     ForgeStateUpdateError, Header)
import           Ouroboros.Consensus.HeaderValidation (OtherHeaderEnvelopeError)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerError)
import           Ouroboros.Consensus.Ledger.Inspect (LedgerEvent, LedgerUpdate, LedgerWarning)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr,  HasTxId,
                     HasTxs (..))
import           Ouroboros.Consensus.Protocol.Abstract (ValidationErr)
import           Ouroboros.Consensus.Shelley.Ledger.Mempool (GenTx, TxId)


-- | Tracing-related constraints for monitoring purposes.
type TraceConstraints blk =
    ( ConvertTxId' blk
    , ConvertTxId blk
    , HasTxs blk
    , HasTxId (GenTx blk)
    , LedgerQueries blk
    , ToJSON   (TxId (GenTx blk))

    , ToObject (ApplyTxErr blk)
    , ToObject (GenTx blk)
    , ToObject (Header blk)
    , ToObject (LedgerError blk)
    , ToObject (LedgerEvent blk)
    , ToObject (OtherHeaderEnvelopeError blk)
    , ToObject (ValidationErr (BlockProtocol blk))
    , ToObject (CannotForge blk)
    , ToObject (ForgeStateUpdateError blk)

    , LogFormatting (LedgerUpdate blk)
    , LogFormatting (LedgerWarning blk)
    , LogFormatting (ApplyTxErr blk)
    , LogFormatting (GenTx blk)
    , LogFormatting (Header blk)
    , LogFormatting (LedgerError blk)
    , LogFormatting (LedgerEvent blk)
    , LogFormatting (OtherHeaderEnvelopeError blk)
    , LogFormatting (ValidationErr (BlockProtocol blk))
    , LogFormatting (CannotForge blk)
    , LogFormatting (ForgeStateUpdateError blk)

    , Show blk
    , Show (Header blk)
    )
