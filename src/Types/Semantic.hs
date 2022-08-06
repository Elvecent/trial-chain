{-|
Module      : Types.Semantic
Copyright   : (c) Cyril Valyavin, 2022
License     : NONE
Maintainer  : kvalyavin@yandex.com
Stability   : experimental
-}

module Types.Semantic
  ( TxId(..)
  , UnspentOutput(..)
  , PublicKey(..)
  , Signature(..)
  , Contract(..)
  , Output(..)
  , Transaction(..)
  , SignedTransaction(..)
  , Block(..)
  ) where

import           Data.List.NonEmpty
import           Data.Map           (Map)
import           GHC.Generics       (Generic)

import qualified Types.Transport    as T

-- | Transaction's ID is really it's hash
newtype TxId = TxId String
  deriving stock (Show, Generic)

data UnspentOutput = UnspentOutput
  { address :: TxId
  , index   :: Int
  } deriving stock (Show, Generic)

newtype PublicKey = PublicKey String
  deriving stock (Show, Eq, Ord, Generic)

newtype Signature = Signature String
  deriving stock (Show, Generic)

-- | A set of requirements for coin redemption
data Contract
  = CheckSig (NonEmpty PublicKey)
  | Trivial
  deriving stock (Show, Generic)

data Output = Output
  { contract :: Contract
  , amount   :: T.Amount
  } deriving stock (Show, Generic)

data Transaction = Transaction
  { inputs     :: [UnspentOutput]
  , outputs    :: NonEmpty Output
  , isCoinbase :: Bool
  } deriving stock (Show, Generic)

data SignedTransaction = SignedTransaction
  { transaction :: Transaction
  , signatures  :: Map PublicKey Signature
  } deriving stock (Show, Generic)

-- | Block's id is actually its hash
newtype BlockId = BlockId String
  deriving stock (Show, Generic)

data Block = Block
  { transactions :: NonEmpty SignedTransaction
  , parent       :: BlockId
  } deriving stock (Show, Generic)
