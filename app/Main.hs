{-# LANGUAGE OverloadedLists #-}

module Main where

import           Control.Lens
import           Data.Binary
import           Data.Foldable
import           Data.List.NonEmpty
import           Data.Text.Lens
import           System.Environment
import           Text.Hex

import           Launch             (runTrialChainServer)
import           Types.Semantic

main :: IO ()
main = do
  -- read hex-encoded transactions from arguments
  -- to put those transactions into the initial block
  txsRaw <- getArgs
  let
    txs = txsRaw
        ^.. traversed
          . to decodeTransaction
          . _Just
  putStrLn "an example coinbase transaction to pass as a parameter:"
  coinbase
    & encodeTransaction
    & putStrLn
  putStrLn "transactions passed:"
  traverse_ (\tx -> putStr "  " >> print tx) txs
  runTrialChainServer (mkBlock txs) 8080
  where
    mkBlock :: [SignedTransaction] -> Maybe Block
    mkBlock txs = do
      netxs <- nonEmpty txs
      Just $ Block netxs (BlockId "origin")

decodeTransaction :: String -> Maybe SignedTransaction
decodeTransaction tx = tx
   &  view packed
   &  decodeHex
  <&> lazyByteString
  <&> decode @SignedTransaction

encodeTransaction :: SignedTransaction -> String
encodeTransaction tx = tx
  & encode
  & strictByteString
  & encodeHex
  & view unpacked

coinbase :: SignedTransaction
coinbase = SignedTransaction
  (Transaction [] [Output Trivial 100] True)
  mempty
