module Types.Semantic.Parse.Validation
  ( ValidationError(..)
  , ValidationErrors
  , Validation
  , runValidation
  , runValidationNoCallStack
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
  :: Monoid e
  => Eff (Error () : State e : es) a
  -> Eff es (Either CallStack a, e)
runValidation m = m
  & runError
  & over (mapped .  _Left) fst
  & runState mempty

runValidationNoCallStack
  :: Monoid e
  => Eff (Error () : State e : es) a
  -> Eff es (Either e a)
runValidationNoCallStack m = m
  & runErrorNoCallStack
  & runState mempty
  & fmap (\(eres, errs) -> eres & _Left .~ errs)

-- | Throw a non-fatal error
dispute
  :: ( Monoid e
    , State e :> es
    , Error () :> es
    )
  => e -> Eff es ()
dispute e = modify (e <>)

-- | Throw a fatal error and exit
refute
  :: ( Monoid e
    , State e :> es
    , Error () :> es
    )
  => e -> Eff es a
refute e = modify (e <>) >> throwError ()
