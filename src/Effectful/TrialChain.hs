{-# LANGUAGE UndecidableInstances #-}

module Effectful.TrialChain where

import           Control.Lens
import           Effectful
import           Effectful.Concurrent.STM
import           Effectful.Dispatch.Dynamic

import           Env
import           Types.Semantic

data TrialChain :: Effect where
  AppendTx :: SignedTransaction -> TrialChain m ()
  BroadcastTx :: SignedTransaction -> TrialChain m TxId
  LookupTx :: TxId -> TrialChain m (Maybe SignedTransaction)

type instance DispatchOf TrialChain = 'Dynamic

appendTx :: TrialChain :> es => SignedTransaction -> Eff es ()
appendTx = send . AppendTx

broadcastTx :: TrialChain :> es => SignedTransaction -> Eff es TxId
broadcastTx = send . BroadcastTx

lookupTx :: TrialChain :> es => TxId -> Eff es (Maybe SignedTransaction)
lookupTx = send . LookupTx

runTrialChainIO
  :: Concurrent :> es
  => TrialChainEnv
  -> Eff (TrialChain : es) a
  -> Eff es a
runTrialChainIO env = interpret $ \_ -> \case
  AppendTx tx -> pure ()
  BroadcastTx tx -> do
    atomically $
      writeTBQueue (env ^. #mempool) tx
    pure $ tx ^. #hashed
  LookupTx txid  -> do
    pure Nothing
