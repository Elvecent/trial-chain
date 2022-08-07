module Launch (runTrialChainServer) where

import           Control.Concurrent.STM
import qualified Control.Immortal         as I
import           Control.Lens
import           Control.Monad
import           Data.Proxy
import qualified Network.Wai.Handler.Warp as W
import           Servant

import           API
import           Env

miningThread :: TrialChainEnv -> IO ()
miningThread env = forever $ do
  tx <- atomically $
    readTBQueue (env ^. #mempool)
  -- TODO append block
  putStrLn $ "appended tx " <> show tx

runTrialChainServer :: W.Port -> IO ()
runTrialChainServer p = do
  env <- newEnv
  void $
    I.create $ \_ -> miningThread env
  W.run p $ serve api $
    hoistServer api (runApp env) server
  where api = Proxy @API
