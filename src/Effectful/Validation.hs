module Effectful.Validation
  ( Validation
  , runValidation
  , runValidationNoCallStack
  , dispute
  , refute
  ) where

import           Control.Lens
import           Effectful
import           Effectful.Error.Static
import           Effectful.State.Static.Local

type Validation e es =
  ( State e :> es
  , Error () :> es
  , Monoid e
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
  :: Validation e es
  => e -> Eff es ()
dispute e = modify (e <>)

-- | Throw a fatal error and exit
refute
  :: Validation e es
  => e -> Eff es a
refute e = modify (e <>) >> throwError ()
