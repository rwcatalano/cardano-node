{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.Benchmarking.Wallet
where
import Prelude

import Control.Concurrent.MVar

import Cardano.Api

import Cardano.Benchmarking.GeneratorTx.Tx as Tx hiding (Fund)
import Cardano.Benchmarking.FundSet as FundSet

type WalletRef = MVar Wallet

data Wallet = Wallet {
    walletNetworkId :: NetworkId
  , walletKey :: SigningKey PaymentKey
  , walletSeqNum :: SeqNumber
  , walletFunds :: FundSet
  }

initWallet :: NetworkId -> SigningKey PaymentKey -> IO (MVar Wallet)
initWallet network key = newMVar $ Wallet {
    walletNetworkId = network
  , walletKey = key
  , walletSeqNum = SeqNumber 1
  , walletFunds = emptyFunds
  }

walletRefInsertFund :: WalletRef -> Fund -> IO ()
walletRefInsertFund ref fund = modifyMVar_  ref $ \w -> return $ walletInsertFund w fund

walletInsertFund :: Wallet -> Fund -> Wallet
walletInsertFund w f
  = w { walletFunds = FundSet.insertFund (walletFunds w) f }

walletDeleteFund :: Wallet -> Fund -> Wallet
walletDeleteFund w f
  = w { walletFunds = FundSet.deleteFund (walletFunds w) f }

walletCreateChange :: forall era. IsShelleyBasedEra era
  => Wallet
  -> [Lovelace]
  -> Either String (Wallet, Tx era)
walletCreateChange wallet values
  = case findSufficientCoin (walletFunds wallet) (sum values) of
    Left err -> Left err
    Right inputCoin -> case genTx (walletKey wallet) (walletNetworkId wallet) inputCoin values of
      Left err -> Left err
      Right (tx, newFunds) -> let
          newWallet = walletDeleteFund
                        (foldl walletInsertFund wallet newFunds)
                        inputCoin
        in Right (newWallet, tx)

genTx :: forall era. IsShelleyBasedEra era
  => SigningKey PaymentKey
  -> NetworkId
  -> Fund 
  -> [Lovelace]
  -> Either String (Tx era, [Fund])
genTx key networkId inFund generateCoins
  = case makeTransactionBody txBodyContent of
      Left err -> error $ show err
      Right b -> Right ( signShelleyTransaction b [WitnessPaymentKey $ getFundKey inFund]
                       , zipWith (mkNewFund $ getTxId b) [TxIx 0 ..] allCoins
                       )
 where
  txBodyContent = TxBodyContent {
        txIns = [(getFundTxIn inFund, BuildTxWith $ KeyWitness KeyWitnessForSpending)]
      , txOuts = map mkTxOut allCoins
      , txFee = mkFee (fromInteger 0)
      , txValidityRange = (TxValidityNoLowerBound, upperBound)
      , txMetadata = TxMetadataNone
      , txAuxScripts = TxAuxScriptsNone
      , txWithdrawals = TxWithdrawalsNone
      , txCertificates = TxCertificatesNone
      , txUpdateProposal = TxUpdateProposalNone
      , txMintValue = TxMintNone
      }

  changeValue = getFundLovelace inFund - sum generateCoins

  allCoins = case compare changeValue 0 of
    GT -> changeValue : generateCoins
    EQ -> generateCoins
    LT -> error "genTX: Bad transaction: insufficient funds"

  mkTxOut v = TxOut addr (mkTxOutValueAdaOnly v)

  mkNewFund :: TxId -> TxIx -> Lovelace -> Fund
  mkNewFund txId txIx val = Fund $ InAnyCardanoEra (cardanoEra @ era) $ FundInEra {
      _fundTxIn = TxIn txId txIx
    , _fundVal = mkTxOutValueAdaOnly val
    , _fundSigningKey = key
    , _fundValidity = Confirmed
    }

  upperBound :: TxValidityUpperBound era
  upperBound = case shelleyBasedEra @ era of
    ShelleyBasedEraShelley -> TxValidityUpperBound ValidityUpperBoundInShelleyEra $ SlotNo maxBound
    ShelleyBasedEraAllegra -> TxValidityNoUpperBound ValidityNoUpperBoundInAllegraEra
    ShelleyBasedEraMary    -> TxValidityNoUpperBound ValidityNoUpperBoundInMaryEra

  addr = Tx.keyAddress @ era networkId key






-- data Step =
--   LocalTx
--  RemoteTx

-- WalletScripts must never store UTxOs.
-- WalletScripts only store references to UTxOs.

-- data WalletScript -- opaque

-- runWalletScript :: WalletScript -> IO (WalletScript, Step)

-- These use Local tx-submission and produce confimed outputs
-- initialSplits :: WalletRef -> IO WalletScript

-- These send txs to a remote nodes mempool.
-- Manage local sequence number / safety interval / UTxO reuse.
-- targetTXs :: WalletRef -> TargetId -> IO WalletScript
