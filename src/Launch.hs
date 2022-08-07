module Launch (runTrialChainServer) where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.STM
import qualified Control.Immortal         as I
import           Control.Lens
import           Control.Monad
import           Data.List.NonEmpty
import           Data.Proxy
import qualified Network.Wai.Handler.Warp as W
import           Servant

import           API
import           Effectful.TrialChain
import           Env
import           Types.Semantic

miningThread :: TrialChainEnv -> IO ()
miningThread env = forever $ do
  threadDelay 10000000
  (txs, hd) <- atomically $ do
    txs <- flushTBQueue (env ^. #mempool)
    hd  <- readTVar (env ^. #head)
    pure (txs, hd)
  case nonEmpty txs of
    Nothing -> pure ()
    Just netxs -> do
      bid <- runApp env $ appendBlock $ Block netxs hd
      putStrLn $ "mined block " <> show bid

runTrialChainServer :: W.Port -> IO ()
runTrialChainServer p = do
  env <- newEnv
  void $
    I.create $ \_ -> miningThread env
  W.run p $ serve api $
    hoistServer api (runApp env) server
  where api = Proxy @API
