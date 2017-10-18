{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}

-- | 'CheckFieldT' and 'CheckField' are specializations of 'CheckT' and 'Check' that associate
-- | errors with "keys" in a map. This is useful for reporting errors in a structured manner.
-- |
-- | For example, if you validated a user's registration payload and found some errors, you may wish
-- | to construct a JSON object mapping @payload_field: [errors]@:
-- |
-- | > recieves:
-- | > { username: "myUsername", email: "notAnEmail", password: "weak" }
-- |
-- | > sends:
-- | > {
-- | >   hasErrors: true,
-- | >   errors: {
-- | >     username: [],
-- | >     email: ["Invalid email"],
-- | >     password: ["Password must be at least 8 characters"]
-- | >   }
-- | > }
module Data.Check.Field
  ( CheckField
  , CheckFieldT
  , checkFieldT
  , checkField
  , C.liftM
  , FieldErrors(..)
  , expectM
  , expect
  , expectAll
  , supposeM
  , suppose
  , whenFalse
  , C.liftEffect
  , err
  , C.failure
  ) where

import Data.Aeson
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Semigroup
import Data.Text (Text)

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

type CheckFieldT m e a b = C.CheckT m (FieldErrors e) a b
type CheckField e a b = CheckFieldT Identity e a b

checkFieldT :: Monad m => CheckFieldT m e a b -> a -> m (Either (FieldErrors e) b)
checkFieldT = C.checkT

checkField :: CheckField e a b -> a -> Either (FieldErrors e) b
checkField = C.check

-- | Runs a predicate, and logs an error for a field if the predicate fails
expectM
  :: Monad m
  => Text          -- ^ Field name
  -> (a -> m Bool) -- ^ Effectful predicate
  -> e             -- ^ Error to log if predicate fails
  -> CheckFieldT m e a a
expectM field p e = C.expectM p (singleError field e)

-- | Like 'expectM' but with a non-effectful predicate
expect :: Applicative m => Text -> (a -> Bool) -> e -> CheckFieldT m e a a
expect field p e = C.expect p (singleError field e)

-- | Combine many predicate-error pairs into a single field check
--
-- > expectAll = foldMap (uncurry expectM)
expectAll :: (Foldable f, Monad m, Semigroup e) => Text -> f (a -> m Bool,e) -> CheckFieldT m e a a
expectAll field = foldMap (uncurry (expectM field))

-- | Runs a predicate, logging some errors for a field if the predicate fails,
-- and returns the result of the predicate
supposeM :: Monad m => Text -> (a -> m Bool) -> e -> CheckFieldT m e a Bool
supposeM field p e = C.supposeM p (singleError field e)

-- | Like 'supposeM' but with a non-effectful predicate
suppose :: Applicative m => Text -> (a -> Bool) -> e -> CheckFieldT m e a Bool
suppose field p e = C.suppose p (singleError field e)

-- | Fail with the specified error(s)
err :: Applicative m => Text -> e -> CheckFieldT m e a b
err k e = C.err $ singleError k e

-- | Logs an error for a field when `False` is passed in
whenFalse :: Applicative m => Text -> e -> CheckFieldT m e Bool Bool
whenFalse field = expect field id
