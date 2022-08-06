module Types.Semantic.Parse.Validation where

import           Control.Lens
import           Effectful
import           Effectful.Error.Static
import           Effectful.State.Static.Local

import           Effectful.TrialChain

type ValidationErrors = [ValidationError]

type Validation es =
  ( State ValidationErrors :> es
  , Error () :> es
  , TrialChain :> es
  )

data ValidationError = ValidationError

runValidation
  :: TrialChain :> es
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
