{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent
import           Control.Lens
import           Effectful
import qualified Streaming.Prelude  as S
import           Test.Tasty
import           Test.Tasty.HUnit

import           Effectful.Client
import           Launch
import qualified Types.Semantic     as S
import           Types.Transport

testPort :: Int
testPort = 8085

main :: IO ()
main = defaultMain $
  withResource (forkIO $
                 runTrialChainServer initialBlock testPort
               ) killThread (const tests)
  where
    initialBlock =
      Just $ S.Block [fst exampleTxs] (S.BlockId "origin")

tests :: TestTree
tests = testGroup "Tests"
  [ testCase "Transactions read back" $
      runScript testPort readAndWriteTxs >>= \case
        Right (txid, tx) -> do
          tx @?= lastTx txid
        Left errs -> assertFailure $ concatMap ('\n' :) errs
  ]

readAndWriteTxs
  :: (IOE :> es, Client :> es)
  => Eff es (TxId, SignedTransaction)
readAndWriteTxs = do
  -- run example transactions in order
  txids <- runStream $
    S.scanM
      (\txid next -> do
          wait
          broadcastTx $ next txid)
      (fst exampleTxs ^. #hashed . to S.transportTxId . to pure)
      pure
      (S.each $ snd exampleTxs)
  let
    secondToLastTxId = head $ drop (length (snd exampleTxs) - 1) txids
  (secondToLastTxId,) <$> getTx (last txids)
  where
    runStream = fmap S.fst' . S.toList
    wait = liftIO $ threadDelay 1500000

exampleTxs :: (S.SignedTransaction, [TxId -> SignedTransaction])
exampleTxs =
  ( gives50ToAlice
  , [ aliceSends40ToBob
    , bobSends10ToAlice
    ]
  )
  where
    gives50ToAlice = S.SignedTransaction
      (S.Transaction []
       [ S.Output (S.CheckSig [S.PublicKey "alice"]) 30
       , S.Output (S.CheckSig [S.PublicKey "alice"]) 20
       ] True)
      mempty

    aliceSends40ToBob txid = SignedTransaction
      (Transaction
        [ UnspentOutput txid 0
        , UnspentOutput txid 1
        ]
        [ Output (CheckSig [PublicKey "bob"]) 40
        , Output (CheckSig [PublicKey "alice"]) 10
        ])
      [(PublicKey "alice", Signature "signedbyalice")]

    bobSends10ToAlice txid = SignedTransaction
      (Transaction
        [ UnspentOutput txid 0
        ]
        [ Output (CheckSig [PublicKey "alice"]) 10
        , Output (CheckSig [PublicKey "bob"]) 30
        ])
      [(PublicKey "bob", Signature "signebybob")]

lastTx :: TxId -> SignedTransaction
lastTx = last $ snd exampleTxs
