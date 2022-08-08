{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Types.Semantic.Parse
  ( parseTxId
  , parseUnspentOutput
  , parsePublicKey
  , parseSignature
  , parseContract
  , parseOutput
  , parseTransaction
  , parseSignedTransaction
  , runValidation
  , ValidationErrors
  ) where

import           Control.Lens         hiding (index)
import           Control.Monad
import           Data.Generics.Labels ()
import           Data.Map             (mapKeys)
import           Effectful
import           Effectful.Concurrent (Concurrent)

import           Effectful.TrialChain
import           Effectful.Validation
import           Types.Semantic
import qualified Types.Transport      as T

type ValidationErrors = [ValidationError]

data ValidationError
  = AmountsDiffer { inputAmount :: T.Amount, outputAmount :: T.Amount }
  | CoinMissing T.UnspentOutput
  | TxMissing   T.TxId
  | SignatureMissing T.PublicKey
  | OtherValidationError
  deriving stock Show

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
  :: ( TrialChain :> es
    , Concurrent :> es
    , Validation ValidationErrors es
    )
  => T.Transaction -> Eff es Transaction
parseTransaction tx = do
  checkAmount
  pure Transaction {..}
  where
    inputs     = tx ^.. #inputs . traversed
                    & traversed %~ parseUnspentOutput
    outputs    = tx ^. #outputs
                    & traversed %~ parseOutput
    -- A user cannot submit a coinbase transaction
    isCoinbase = False
    checkAmount = do
      inputAmount <- sum <$> traverse (lookupCoin 0 #amount) inputs
      unless (inputAmount == outputAmount) $
        refute [AmountsDiffer inputAmount outputAmount]
    outputAmount = outputs & sumOf (traversed . #amount)

parseSignedTransaction
  :: ( TrialChain :> es
    , Concurrent :> es
    , Validation ValidationErrors es
    )
  => T.SignedTransaction -> Eff es SignedTransaction
parseSignedTransaction stx = do
  transaction <- stx ^. #transaction . to parseTransaction
  checkSignatures transaction
  pure SignedTransaction {..}
  where
    signatures = stx
      ^. #signatures
      . to (mapKeys parsePublicKey)
      . to (fmap parseSignature)
    checkSignatures transaction = do
      pks <- pubKeys transaction
      void $ traverse checkSignature pks
    checkSignature pk = case signatures ^? ix (pk :: PublicKey) of
      Nothing -> refute [SignatureMissing $ transportPublicKey pk]
      Just _  -> pure () -- We will not actually check signatures, only their presence
    pubKeys tx = do
      cs <- traverse (lookupCoin Trivial #contract) (tx ^. #inputs)
      pure $ cs ^.. traversed . #_CheckSig . traversed

lookupCoin
  :: ( TrialChain :> es
    , Concurrent :> es
    , Validation ValidationErrors es
    )
  => d -> _optic -> UnspentOutput -> Eff es d
lookupCoin def fld uo = do
  let txid = uo ^. #address
  mtx <- lookupTx txid
  case mtx of
    Nothing -> dispute [TxMissing $ transportTxId txid] >> pure def
    Just tx' ->
      case tx' ^? #transaction . #outputs . ix (uo ^. #index) . fld of
        Nothing -> dispute [CoinMissing $ transportUnspentOutput uo] >> pure def
        Just x  -> pure x
