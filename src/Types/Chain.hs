module Types.Chain where

import           Control.Concurrent.STM
import           GHC.Generics           (Generic)
import qualified StmContainers.Map      as M

import           Types.Semantic

type TrialChainBlocks = M.Map BlockId Block

data TrialChainEnv = TrialChainEnv
  { mempool :: TBQueue SignedTransaction
  , chain   :: TrialChainBlocks
  , head    :: TVar BlockId
  } deriving stock (Generic)
