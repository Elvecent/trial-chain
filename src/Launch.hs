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

miningThread :: Maybe Block -> TrialChainEnv -> IO ()
miningThread initialBlock env = do
  case initialBlock of
    Nothing -> pure ()
    Just  b -> do
      runApp env $ appendBlock b
      putStr "\nmined initial block "
      print (b ^. #hashed :: BlockId)
      putStrLn "with transactions"
      void $ traverse (\txid -> putStr "  " >> print txid) (
        (b ^. #transactions & traversed %~ view #hashed)
        :: NonEmpty TxId)
  forever $ do
    let seconds = 1
    (txs, hd) <- atomically $ do
      txs <- flushTBQueue (env ^. #mempool)
      hd  <- readTVar (env ^. #head)
      pure (txs, hd)
    case nonEmpty txs of
      Nothing -> pure ()
      Just netxs -> do
        bid <- runApp env $ appendBlock $ Block netxs hd
        putStrLn $ "mined block " <> show bid
        threadDelay $ seconds * 1000000

runTrialChainServer :: Maybe Block -> W.Port -> IO ()
runTrialChainServer initialBlock p = do
  env <- newEnv
  void $
    I.create $ \_ -> miningThread initialBlock env
  W.run p $ serve api $
    hoistServer api (runApp env) server
  where api = Proxy @API
