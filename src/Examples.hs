{-|
Module      : Examples
Copyright   : (c) Cyril Valyavin, 2022
License     : NONE
Maintainer  : kvalyavin@yandex.com
Stability   : experimental

Sample data to use in GHCi
-}

{-# LANGUAGE OverloadedLists #-}

module Examples where

import           Types.Semantic

charlieKey :: PublicKey
charlieKey = PublicKey "charlie"

charlieSignature :: Signature
charlieSignature = Signature "signedbycharlie"

charlieMined :: SignedTransaction
charlieMined = SignedTransaction (Transaction [] [o] True) []
  where
    o = Output (CheckSig [PublicKey "charlie"]) 100

charliePaid :: SignedTransaction
charliePaid = SignedTransaction
  (Transaction [i] [o, c] False)
  [(charlieKey, charlieSignature)]
  where
    i = UnspentOutput (TxId "123") 0
    o = Output (CheckSig [PublicKey "alice", PublicKey "bob"]) 80
    c = Output (CheckSig [PublicKey "charlie"]) 20
