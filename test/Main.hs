module Main (main) where

import           API
import           Control.Concurrent
import           Launch
import           Servant.Client

main :: IO ()
main = do
  serverThread <- forkIO $ runTrialChainServer 8085
  killThread serverThread
