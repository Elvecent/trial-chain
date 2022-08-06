{-# LANGUAGE UndecidableInstances #-}

module Effectful.TrialChain where

import           Effectful
import           Effectful.Dispatch.Dynamic

import           Env
import           Types.Semantic

data TrialChain :: Effect where
  AppendTx :: SignedTransaction -> TrialChain m TxId
  LookupTx :: TxId -> TrialChain m (Maybe SignedTransaction)

type instance DispatchOf TrialChain = 'Dynamic

appendTx :: TrialChain :> es => SignedTransaction -> Eff es TxId
appendTx = send . AppendTx

lookupTx :: TrialChain :> es => TxId -> Eff es (Maybe SignedTransaction)
lookupTx = send . LookupTx

runTrialChainIO
  :: IOE :> es
  => TrialChainEnv
  -> Eff (TrialChain : es) a
  -> Eff es a
runTrialChainIO env = interpret $ \_ -> \case
  AppendTx _ -> pure $ TxId mempty
  LookupTx _ -> pure undefined
