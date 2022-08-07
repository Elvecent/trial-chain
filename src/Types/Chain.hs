module Types.Chain where

import           Control.Concurrent.STM
import           GHC.Generics           (Generic)
import qualified StmContainers.Map      as M

import           Types.Semantic

type TrialChainBlocks = M.Map TxId Block

data TrialChainEnv = TrialChainEnv
  { mempool :: TBQueue SignedTransaction
  , chain   :: TrialChainBlocks
  } deriving stock (Generic)
