module Types.Semantic.Parse.Validation
  ( ValidationError(..)
  , ValidationErrors
  , Validation
  , runValidation
  , dispute
  , refute
  ) where

import           Control.Lens
import           Effectful
import           Effectful.Error.Static
import           Effectful.State.Static.Local

import           Effectful.TrialChain
import           Types.Transport

type ValidationErrors = [ValidationError]

data ValidationError
  = AmountsDiffer Amount Amount
  | CoinMissing UnspentOutput
  | TxMissing   TxId
  | SignatureMissing PublicKey
  | OtherValidationError
  deriving stock Show

type Validation es =
  ( State ValidationErrors :> es
  , Error () :> es
  , TrialChain :> es
  , IOE :> es
  )

runValidation
  :: (TrialChain :> es, IOE :> es)
  => Eff (Error () : State ValidationErrors : es) a
  -> Eff es (Either CallStack a, ValidationErrors)
runValidation m = m
  & runError
  & over (mapped .  _Left) fst
  & runState mempty

-- | Throw a non-fatal error
dispute
  :: Validation es
  => ValidationError -> Eff es ()
dispute e = modify ((:) e)

-- | Throw a fatal error and exit
refute
  :: Validation es
  => ValidationError -> Eff es ()
refute e = modify ((:) e) >> throwError ()
