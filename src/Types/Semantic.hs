{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Types.Semantic
Copyright   : (c) Cyril Valyavin, 2022
License     : NONE
Maintainer  : kvalyavin@yandex.com
Stability   : experimental
-}

module Types.Semantic
  ( TxId(..)
  , transportTxId
  , UnspentOutput(..)
  , transportUnspentOutput
  , PublicKey(..)
  , transportPublicKey
  , Signature(..)
  , transportSignature
  , Contract(..)
  , transportContract
  , Output(..)
  , transportOutput
  , Transaction(..)
  , transportTransaction
  , SignedTransaction(..)
  , hashHexString
  , txSerialize
  , txHash
  , transportSignedTransaction
  , BlockId(..)
  , Block(..)
  , blockHash
  ) where

import           Control.Lens
import           Crypto.Hash.SHA256      (hash)
import           Data.Binary
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8   as B
import           Data.ByteString.Lens
import           Data.Generics.Labels    ()
import           Data.Hashable           (Hashable)
import           Data.List.NonEmpty
import           Data.Map                (Map, mapKeys)
import           GHC.Generics            (Generic)
import           GHC.OverloadedLabels

import qualified Types.Transport         as T

-- | Transaction's ID is really it's hash
newtype TxId = TxId String
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary)

transportTxId :: TxId -> T.TxId
transportTxId = view coerced

data UnspentOutput = UnspentOutput
  { address :: TxId
  , index   :: Int
  } deriving stock (Show, Generic)
    deriving anyclass (Binary)

transportUnspentOutput :: UnspentOutput -> T.UnspentOutput
transportUnspentOutput uo = T.UnspentOutput
  { T.address = uo ^. #address . to transportTxId
  , T.index   = uo ^. #index
  }

newtype PublicKey = PublicKey String
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary)

transportPublicKey :: PublicKey -> T.PublicKey
transportPublicKey = view coerced

newtype Signature = Signature String
  deriving stock (Show, Generic)
  deriving anyclass (Binary)

transportSignature :: Signature -> T.Signature
transportSignature = view coerced

-- | A set of requirements for coin redemption
data Contract
  = CheckSig (NonEmpty PublicKey)
  | Trivial
  deriving stock (Show, Generic)
  deriving anyclass (Binary)

transportContract :: Contract -> T.Contract
transportContract = \case
  Trivial      -> T.Trivial
  CheckSig pks -> pks & fmap transportPublicKey & T.CheckSig

data Output = Output
  { contract :: Contract
  , amount   :: T.Amount
  } deriving stock (Show, Generic)
    deriving anyclass (Binary)

transportOutput :: Output -> T.Output
transportOutput o = T.Output
  { T.contract = o ^. #contract . to transportContract
  , T.amount   = o ^. #amount
  }

data Transaction = Transaction
  { inputs     :: [UnspentOutput]
  , outputs    :: NonEmpty Output
  , isCoinbase :: Bool
  } deriving stock (Show, Generic)
    deriving anyclass (Binary)

transportTransaction :: Transaction -> T.Transaction
transportTransaction tx = T.Transaction
  { T.inputs = tx ^. #inputs & fmap transportUnspentOutput
  , T.outputs = undefined
  }

data SignedTransaction = SignedTransaction
  { transaction :: Transaction
  , signatures  :: Map PublicKey Signature
  } deriving stock (Show, Generic)
    deriving anyclass (Binary)

hashHexString :: B.ByteString -> String
hashHexString bs = bs
  & B.byteStringHex
  & B.toLazyByteString
  & B.toStrict
  & B.unpack

txSerialize :: Transaction -> B.ByteString
txSerialize = B.toStrict . encode

txHash :: Transaction -> TxId
txHash tx = tx
  & txSerialize
  & hash
  & hashHexString
  & TxId

instance {-# OVERLAPPING #-}
  (Contravariant f)
  => IsLabel "hashed" (Optic' (->) f Transaction TxId) where
  fromLabel = to txHash

instance {-# OVERLAPPING #-}
  ( Contravariant f
  , Functor f
  )
  => IsLabel "hashed" (Optic' (->) f SignedTransaction TxId) where
  fromLabel = #transaction . to txHash

transportSignedTransaction :: SignedTransaction -> T.SignedTransaction
transportSignedTransaction st = T.SignedTransaction
  { T.transaction = st ^. #transaction . to transportTransaction
  , T.signatures  = st ^. #signatures
                        & mapKeys transportPublicKey
                        & fmap transportSignature
  }

-- | Block's id is actually its hash
newtype BlockId = BlockId String
  deriving stock (Show, Eq, Generic)
  deriving newtype Hashable

data Block = Block
  { transactions :: NonEmpty SignedTransaction
  , parent       :: BlockId
  } deriving stock (Show, Generic)

blockHash :: Block -> BlockId
blockHash b = block
  & hash
  & hashHexString
  & BlockId
  where
    block = (b ^.. #transactions . traversed . #transaction
                 & foldMap txSerialize)
         <> (b ^. #parent . #_BlockId . packedChars)

instance {-# OVERLAPPING #-}
  (Contravariant f)
  => IsLabel "hashed" (Optic' (->) f Block BlockId) where
  fromLabel = to blockHash
