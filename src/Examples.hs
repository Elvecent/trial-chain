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

import           Control.Lens

import           Types.Semantic

charlieKey :: PublicKey
charlieKey = PublicKey "charlie"

aliceKey :: PublicKey
aliceKey = PublicKey "alice"

bobKey :: PublicKey
bobKey = PublicKey "bob"

charlieSignature :: Signature
charlieSignature = Signature "signedbycharlie"

aliceSignature :: Signature
aliceSignature = Signature "signedbyalice"

bobSignature :: Signature
bobSignature = Signature "signedbybob"

charlieMined :: SignedTransaction
charlieMined = SignedTransaction (Transaction [] [o] True) []
  where
    o = Output (CheckSig [charlieKey]) 100

charliePaid :: SignedTransaction
charliePaid = SignedTransaction
  (Transaction [i] [o, c] False)
  [(charlieKey, charlieSignature)]
  where
    i = UnspentOutput (charlieMined ^. #hashed) 0
    o = Output (CheckSig [aliceKey, bobKey]) 80
    c = Output (CheckSig [charlieKey]) 20

charlieReceived :: SignedTransaction
charlieReceived = SignedTransaction
  (Transaction [i] [o] False)
  [(bobKey, bobSignature), (aliceKey, aliceSignature)]
  where
    i = UnspentOutput (charliePaid ^. #hashed) 0
    o = Output (CheckSig [charlieKey]) 80

block0 :: Block
block0 = Block [charlieMined] (BlockId "origin")

block1 :: Block
block1 = Block [charliePaid] (block0 ^. #hashed)

block2 :: Block
block2 = Block [charlieReceived] (block1 ^. #hashed)
