module Launch (runTrialChainServer) where

import           Data.Proxy
import qualified Network.Wai.Handler.Warp as W
import           Servant

import           API
import           Env

runTrialChainServer :: W.Port -> IO ()
runTrialChainServer p = do
  env <- newEnv
  W.run p $ serve api $
    hoistServer api (runApp env) server
  where api = Proxy @API
