{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent
import           Data.Proxy
import           Network.HTTP.Client hiding (Proxy)
import           Servant.API
import           Servant.Client
import           Test.Tasty
import           Test.Tasty.HUnit

import           API
import           Launch
import           Types.Transport

main :: IO ()
main = defaultMain $
  withResource (forkIO $ runTrialChainServer 8085) killThread (const tests)

tests :: TestTree
tests = testGroup "Tests"
  [ testCase "Transaction reads back" $ script >>= \etx ->
      etx @?= Right exampleTx
  ]

script :: IO (Either String SignedTransaction)
script = do
  manager' <- newManager defaultManagerSettings
  let
    env = mkClientEnv manager' (BaseUrl Http "localhost" 8085 "")
  etxId <- runClientM (broadcastTx exampleTx) env
  case etxId of
    Left err -> pure $ Left $ show err
    Right txId -> do
      threadDelay 1500000
      etx <- runClientM (getTx txId) env
      case etx of
        Left err -> pure $ Left $ show err
        Right Nothing -> pure $ Left "failed to retrieve transaction"
        Right (Just tx) ->
          pure $ Right tx

broadcastTx :: SignedTransaction -> ClientM TxId
getTx :: TxId -> ClientM (Maybe SignedTransaction)
broadcastTx :<|> getTx = client (Proxy @API)

exampleTx :: SignedTransaction
exampleTx = SignedTransaction
  (Transaction [] [o])
  mempty
  where
    o = Output Trivial 0
