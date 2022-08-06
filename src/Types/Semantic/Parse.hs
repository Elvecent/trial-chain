{-# LANGUAGE RecordWildCards #-}
module Types.Semantic.Parse where

import           Control.Lens           hiding (index)
import           Control.Monad.Validate
import           Data.Generics.Labels   ()
import           Data.Map               (mapKeys)

import           Types.Semantic
import qualified Types.Transport        as T

data ValidationError = ValidationError

parseTxId :: T.TxId -> TxId
parseTxId = view coerced

parseUnspentOutput :: T.UnspentOutput -> UnspentOutput
parseUnspentOutput uo =
  let
    address = uo ^. #address . to parseTxId
    index   = uo ^. #index
  in
  UnspentOutput {..}

parsePublicKey :: T.PublicKey -> PublicKey
parsePublicKey = view coerced

parseSignature :: T.Signature -> Signature
parseSignature = view coerced

parseContract :: T.Contract -> Contract
parseContract = \case
  T.Trivial     -> Trivial
  T.CheckSig ks -> ks
    & traversed %~ parsePublicKey
    & CheckSig

parseOutput :: T.Output -> Output
parseOutput o =
  let
    contract = o ^. #contract . to parseContract
    amount   = o ^. #amount
  in
  Output {..}

parseTransaction
  :: MonadValidate ValidationError m
  => T.Transaction -> m Transaction
parseTransaction tx = do
  let
    inputs     = tx ^.. #inputs . traversed
                    & traversed %~ parseUnspentOutput
    outputs    = tx ^. #outputs
                    & traversed %~ parseOutput
    -- A user cannot submit a coinbase transaction
    isCoinbase = False
  pure Transaction {..}

parseSignedTransaction
  :: MonadValidate ValidationError m
  => T.SignedTransaction -> m SignedTransaction
parseSignedTransaction stx = do
  let
    signatures  = stx ^. #signatures
                       . to (mapKeys parsePublicKey)
                       . to (fmap parseSignature)
  transaction <- stx ^. #transaction . to parseTransaction
  pure SignedTransaction {..}
