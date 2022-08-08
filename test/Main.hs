{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent
import           Effectful
import           Test.Tasty
import           Test.Tasty.HUnit

import           Effectful.Client
import           Launch
import           Types.Transport

testPort :: Int
testPort = 8085

main :: IO ()
main = defaultMain $
  withResource (forkIO $ runTrialChainServer testPort) killThread (const tests)

tests :: TestTree
tests = testGroup "Tests"
  [ testCase "Transaction reads back" $
      runScript testPort script >>= \etx ->
        etx @?= Right exampleTx
  ]

script
  :: (IOE :> es, Client :> es)
  => Eff es SignedTransaction
script = do
  txid <- broadcastTx exampleTx
  liftIO $ threadDelay 1500000
  getTx txid

exampleTx :: SignedTransaction
exampleTx = SignedTransaction
  (Transaction [] [o])
  mempty
  where
    o = Output Trivial 0
