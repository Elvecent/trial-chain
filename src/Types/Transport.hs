{-# LANGUAGE StrictData #-}

module Types.Transport
  ( TxId(..)
  , UnspentOutput(..)
  , Amount
  , PublicKey(..)
  , Contract(..)
  , Output(..)
  , Transaction(..)
  , Signature(..)
  , SignedTransaction(..)) where

import           Data.Aeson
import           Data.Fixed
import           Data.List.NonEmpty
import           Data.Map           (Map)
import           GHC.Generics       (Generic)
import           Servant

newtype TxId = TxId String
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype ( FromHttpApiData
                   , ToHttpApiData
                   )

data UnspentOutput = UnspentOutput
  { address :: TxId
  , index   :: Int
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

type Amount = Micro

newtype PublicKey = PublicKey String
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

jsonTaggedSum :: Options
jsonTaggedSum = defaultOptions
  { sumEncoding = TaggedObject "type" "contents"
  , tagSingleConstructors = True
  }

data Contract
  = CheckSig (NonEmpty PublicKey)
  | Trivial
  deriving stock (Show, Eq, Generic)

instance ToJSON Contract where
  toJSON = genericToJSON jsonTaggedSum

instance FromJSON Contract where
  parseJSON = genericParseJSON jsonTaggedSum

data Output = Output
  { contract :: Contract
  , amount   :: Amount
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Transaction = Transaction
  { inputs  :: [UnspentOutput]
  , outputs :: NonEmpty Output
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype Signature = Signature String
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SignedTransaction = SignedTransaction
  { transaction :: Transaction
  , signatures  :: Map PublicKey Signature
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
