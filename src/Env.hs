{-# LANGUAGE RecordWildCards #-}
module Env
  ( module Types.Chain
  , newEnv
  ) where

import           Control.Concurrent.STM
import           Prelude                hiding (head)
import qualified StmContainers.Map      as M

import           Types.Chain
import           Types.Semantic

newEnv :: IO TrialChainEnv
newEnv = do
  mempool <- newTBQueueIO 10
  chain   <- M.newIO
  head    <- newTVarIO $ BlockId "origin"
  return TrialChainEnv {..}
