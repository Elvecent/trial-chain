{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}

module Effectful.TrialChain where

import           Control.Lens
import           Effectful
import           Effectful.Concurrent.STM
import           Effectful.Dispatch.Dynamic
import           Prelude                    hiding (head)
import           StmContainers.Map          as M
import           Streaming
import qualified Streaming.Prelude          as S

import           Env
import           Types.Semantic

data SomeConcurrent a = SomeConcurrent
  { runSomeConcurrent
      :: forall es. Concurrent :> es
      => Eff es a
  } deriving stock (Functor)

instance Applicative SomeConcurrent where
  pure x = SomeConcurrent $ pure x
  (SomeConcurrent ff) <*> (SomeConcurrent fx) = SomeConcurrent (ff <*> fx)

instance Monad SomeConcurrent where
  (SomeConcurrent mx) >>= mf = SomeConcurrent $ do
    x <- mx
    runSomeConcurrent (mf x)

data TrialChain :: Effect where
  -- | Write a block to the chain
  AppendBlock :: Block -> TrialChain m BlockId
  -- | Enqueue a transaction to the mempool
  BroadcastTx :: SignedTransaction -> TrialChain m TxId
  -- | Create an iterator over blocks
  GetBlocks   :: TrialChain m (Stream (Of Block) SomeConcurrent ())

type instance DispatchOf TrialChain = 'Dynamic

appendBlock :: TrialChain :> es => Block -> Eff es BlockId
appendBlock = send . AppendBlock

broadcastTx :: TrialChain :> es => SignedTransaction -> Eff es TxId
broadcastTx = send . BroadcastTx

getBlocks :: TrialChain :> es => Eff es (Stream (Of Block) SomeConcurrent ())
getBlocks = send GetBlocks

runTrialChainIO
  :: forall es a
  . Concurrent :> es
  => TrialChainEnv
  -> Eff (TrialChain : es) a
  -> Eff es a
runTrialChainIO env = interpret $ \_ -> \case
  AppendBlock b -> do
    atomically $ do
      M.insert b blockId chain
      writeTVar head blockId
    pure blockId
    where
      blockId = b ^. #hashed

  BroadcastTx tx -> do
    atomically $
      writeTBQueue mempool tx
    pure txid
    where
      txid = tx ^. #hashed

  GetBlocks -> do
    headBlock <- atomically $ do
      hd <- readTVar head
      M.lookup hd chain
    nextBlock headBlock
      & pure
    where
      nextBlock :: Maybe Block -> Stream (Of Block) SomeConcurrent ()
      nextBlock current = do
        case current of
          Just b -> do
            next <- lift $ SomeConcurrent $ atomically $
              M.lookup (b ^. #parent) chain
            S.yield b
            nextBlock next
          Nothing -> pure ()
  where
    TrialChainEnv {..} = env

lookupTx
  :: forall es.
    ( Concurrent :> es
    , TrialChain :> es
    )
  => TxId -> Eff es (Maybe SignedTransaction)
lookupTx txid = do
  blocks <- getBlocks
  let res =
        blocks
        & S.map findTx
        & S.filter isJust
        & S.head
        & fmap flatten
  runSomeConcurrent res
  where
    findTx block = block
      ^? #transactions
      . traversed
      . filteredBy (#hashed . (only txid))
    isJust = \case
      Nothing -> False
      _       -> True
    flatten = join . S.fst'
