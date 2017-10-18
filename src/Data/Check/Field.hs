{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}

-- | 'CheckFieldT' and 'CheckField' are specializations of 'CheckT' and 'Check', that associate
-- errors with "keys" in a map. This is useful for reporting errors in a structured manner.
--
-- For example, if you validated a user's registration payload and found some errors, you may wish
-- to construct a JSON object mapping @payload_field: [errors]@:
--
-- > receives:
-- > { username: "myUsername", email: "notAnEmail", password: "weak" }
--
-- > sends:
-- > {
-- >   hasErrors: true,
-- >   errors: {
-- >     username: [],
-- >     email: ["Invalid email"],
-- >     password: ["Password must be at least 8 characters"]
-- >   }
-- > }
module Data.Check.Field
  ( CheckField
  , CheckFieldT
  , FieldErrors(..)
  , runCheckFieldT
  , runCheckField
  , C.liftEffect
  , expectM
  , expect
  , expectAll
  , whenFalse
  , err
  ) where

import Data.Aeson
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Semigroup
import Data.Text (Text)
import Data.These

import qualified Data.Map as M

import qualified Data.Check as C

newtype FieldErrors e = FieldErrors { getFieldErrors :: Map Text (NonEmpty e) }
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

instance Semigroup (FieldErrors e) where
  (<>) (FieldErrors ma) (FieldErrors mb) = FieldErrors $ M.unionWith (<>) ma mb

instance Monoid (FieldErrors e) where
  mempty = FieldErrors M.empty
  mappend = (<>)

singleError :: Text -> e -> FieldErrors e
singleError field e = FieldErrors . M.singleton field $ pure e

instance ToJSON e => ToJSON (FieldErrors e) where
  toJSON (FieldErrors e) = object . fmap transformError $ M.toList e
    where
      transformError (fieldName, e :| []) = fieldName .= e
      transformError (fieldName, e :| errs) = fieldName .= toJSON (e:errs)

type CheckFieldT m e i o = C.CheckT m (FieldErrors e) i o
type CheckField e i o = CheckFieldT Identity e i o

-- | Get the results of running an effectful check on an input
runCheckFieldT :: Monad m => CheckFieldT m e i o -> i -> m (These (FieldErrors e) o)
runCheckFieldT = C.runCheckT

-- | Get the results of running a non-effectful check on an input
runCheckField :: CheckField e i o -> i -> These (FieldErrors e) o
runCheckField = C.runCheck

-- | Runs a predicate, and logs an error for a field if the predicate fails
--
-- Returns its input
expectM
  :: Monad m
  => Text          -- ^ Field name
  -> (i -> m Bool) -- ^ Effectful predicate
  -> e             -- ^ Error to log if predicate fails
  -> CheckFieldT m e i i
expectM field p e = C.expectM p (singleError field e)

-- | Like 'expectM' but with a non-effectful predicate
expect :: Monad m => Text -> (i -> Bool) -> e -> CheckFieldT m e i i
expect field p e = C.expect p (singleError field e)

-- | Logs an error for a field when `False` is passed in
--
-- Returns `True` on success, and produces no input (a `This`) on failure
--
-- > whenFalse = expect id
whenFalse :: Monad m => Text -> e -> CheckFieldT m e Bool Bool
whenFalse field = expect field id

-- | Combine many `expect`ments into a single field check
--
-- > expectAll = foldMap (uncurry expectM)
expectAll :: (Foldable f, Monad m, Semigroup e) => Text -> f (i -> m Bool,e) -> CheckFieldT m e i i
expectAll field = foldMap (uncurry (expectM field))

-- | Fail with the specified error(s), producing no output
err :: Monad m => Text -> e -> CheckFieldT m e i o
err k e = C.err $ singleError k e

