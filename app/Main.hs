module Main where

import           Examples
import           Launch   (runTrialChainServer)

main :: IO ()
main = do
  print charliePaid
  runTrialChainServer Nothing 8080
