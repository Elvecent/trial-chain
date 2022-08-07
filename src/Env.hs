{-# LANGUAGE RecordWildCards #-}
module Env
  ( module Types.Chain
  , newEnv
  ) where

import           Control.Concurrent.STM
import qualified StmContainers.Map      as M

import           Types.Chain

newEnv :: IO TrialChainEnv
newEnv = do
  mempool <- newTBQueueIO 10
  chain   <- M.newIO
  return TrialChainEnv {..}
