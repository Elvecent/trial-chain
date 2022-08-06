module Types.Chain where

import           Control.Concurrent.STM
import qualified StmContainers.Map      as M

import           Types.Semantic

type TrialChainBlocks = M.Map TxId Block

data TrialChainEnv = TrialChainEnv
  { txQueue :: TBQueue SignedTransaction
  , chain   :: TrialChainBlocks
  }
