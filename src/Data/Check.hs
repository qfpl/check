{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Check is used to "check" inputs and accumulate errors on failure
--
-- This is done by building a `CheckT` computation, which contains all the rules that some data
-- should satisfy, then using `checkT` to run it with some input. If errors occurred
-- they are accumulated and returned, otherwise an output is returned.
--
-- The most effective way to build a `CheckT` computation is using 'Arrow's.
--
-- > {-# language Arrows #-}
-- >
-- > import Data.Check
-- > import Data.List
-- >
-- > data User = User { username :: String, password :: String, email :: String }
-- >
-- > data UserError
-- >   = EmailInvalid
-- >   | UsernameNotAllowed
-- >   | PasswordTooWeak
-- >   deriving (Eq, Show)
-- >
-- > validEmail = not . isPrefixOf "invalid"
-- > strongPassword = (>= 8) . length
-- >
-- > usernameAllowed :: String -> IO Bool
-- > usernameAllowed username = do
-- >   let usernamesInDatabase = ["foo", "bar", "baz"] -- Imagine this is a database lookup
-- >   return $ username `notElem` usernamesInDatabase
-- >
-- > validateUser :: CheckT IO [UserError] User User
-- > validateUser = proc user -> do
-- >   expectM (usernameAllowed . username) [UsernameNotAllowed] -< user
-- >   expect (strongPassword . password) [PasswordTooWeak] -< user
-- >   expect (validEmail . email) [EmailInvalid] -< user
-- >   returnA -< user
--
-- >>> checkT validateUser (User "allowed_username" "weak" "invalid@email.com")
-- These [PasswordTooWeak, EmailInvalid] (User "allowed_username" "weak" "invalid@email.com")
--
-- >>> checkT validateUser (User "allowed_username" "strong_password" "valid@email.com")
-- That (User "allowed_username" "strong_password" "valid_email")
--
-- A `CheckT` computation can have an output type that is different to its input type,
-- as well as letting intermediate results influence the flow of checking
--
-- > data LoginPayload = LoginPayload { loginUsername :: String, loginPassword :: String }
-- >
-- > data LoginError = UserNotFound PasswordIncorrect
-- >
-- > lookupUser :: String -> IO (Maybe User)
-- > lookupUser name = ...
-- >
-- > validateLogin :: CheckT IO [LoginError] LoginPayload User
-- > validateLogin = proc payload -> do
-- >   maybeUser <- liftEffect lookupUser -< loginUsername payload
-- >   case maybeUser of
-- >     Just user -> do
-- >       whenFalse [PasswordIncorrect] -< password user == loginPassword payload
-- >       returnA -< user
-- >     Nothing -> err [UserNotFound] -< ()
--
-- >>> checkT validateLogin (LoginPayload "incorrect_username" "incorrect_password")
-- This [UserNotFound]
--
-- >>> checkT validateLogin (LoginPayload "correct_username" "incorrect_password")
-- These [PasswordIncorrect] (LoginPayload "correct_username" "incorrect_password")
--
-- >>> checkT validateLogin (LoginPayload "correct_username" "correct_password")
-- That (User "correct_username" "correct_password" "user@email.com")

module Data.Check
  ( CheckT(..)
  , Check
  , runCheckT
  , runCheck
  , liftEffect
  , expectM
  , expect
  , expectAll
  , whenFalse
  , err
  , failure
  ) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad.Chronicle
import Data.Functor.Identity
import Data.Profunctor
import Data.Semigroup
import Data.These
import Prelude hiding (id, (.))

-- | 'CheckT' transforms an @input@ into an @output@ inside some @m@ while potentially generating @error@s
newtype CheckT m error input output
  = CheckT { unCheckT :: Kleisli (ChronicleT error m) input output }
  deriving
    ( Category
    , Arrow
    , ArrowChoice
    , ArrowZero
    , ArrowPlus
    , ArrowApply
    , Profunctor
    )

-- | Non-effectful checking
type Check error input output = CheckT Identity error input output

-- | Get the results of running an effectful check on an input
runCheckT :: Monad m => CheckT m e i o -> i -> m (These e o)
runCheckT ab a = runChronicleT $ runKleisli (unCheckT ab) a

-- | Get the results of running a non-effectful check on an input
runCheck :: Check e i o -> i -> These e o
runCheck c a = runIdentity $ runCheckT c a

instance Functor m => Functor (CheckT m e i) where
  fmap f (CheckT (Kleisli m)) = CheckT . Kleisli $ fmap f <$> m

instance (Applicative m, Semigroup e) => Applicative (CheckT m e i) where
  pure = CheckT . Kleisli . const . pure
  (CheckT (Kleisli f)) <*> (CheckT (Kleisli b)) =
    CheckT . Kleisli $ liftA2 (<*>) f b

instance (Monad m, Semigroup e) => Monad (CheckT m e i) where
  (CheckT (Kleisli m)) >>= f =
    CheckT . Kleisli $ \i -> m i >>= (($ i) . runKleisli . unCheckT . f)

-- | Lift an effectful function into a 'CheckT' computation
liftEffect :: Functor m => (i -> m o) -> CheckT m e i o
liftEffect f = CheckT $ Kleisli (ChronicleT . fmap That . f)

instance (Monad m, Semigroup e) => Monoid (CheckT m e i i) where
  mempty = id
  mappend = (.)

-- | Runs a predicate, and logs an error if the predicate fails
--
-- Returns its input
expectM
  :: (Monad m, Semigroup e)
  => (i -> m Bool)  -- ^ Effectful predicate
  -> e              -- ^ Errors to log if predicate fails
  -> CheckT m e i i
expectM p e =
  CheckT .
  Kleisli $ \a -> do
    res <- lift $ p a
    if res
      then pure a
      else chronicle $ These e a

-- | Like 'expectM' but with a non-effectful predicate
expect :: (Monad m, Semigroup e) => (i -> Bool) -> e -> CheckT m e i i
expect p e =
  CheckT .
  Kleisli $ \a ->
    if p a
    then pure a
    else chronicle $ These e a

-- | Combine many `expect`ments into a single check
--
-- > expectAll = foldMap (uncurry expectM)
expectAll :: (Foldable f, Monad m, Semigroup e) => f (i -> m Bool, e) -> CheckT m e i i
expectAll = foldMap (uncurry expectM)

-- | Logs an error when `False` is passed in
--
-- Returns `True` on success, and produces no input (a `This`) on failure
--
-- > whenFalse = expect id
whenFalse :: (Monad m, Semigroup e) => e -> CheckT m e Bool Bool
whenFalse = expect id

-- | Fail with the specified error(s), producing no output
err :: (Monad m, Semigroup e) => e -> CheckT m e i o
err = CheckT . Kleisli . const . confess

-- | Fail on any input, producing no output
--
-- > failure = err mempty
failure :: (Monad m, Semigroup e, Monoid e) => CheckT m e i o
failure = err mempty
