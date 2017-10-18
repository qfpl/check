{-# LANGUAGE FlexibleInstances #-}

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
-- Left [PasswordTooWeak, EmailInvalid]
--
-- >>> checkT validateUser (User "allowed_username" "strong_password" "valid@email.com")
-- Right (User "allowed_username" "strong_password" "valid_email")
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
-- >   maybeUser <- liftM lookupUser -< loginUsername payload
-- >   expect isJust [UserNotFound] -< maybeUser
-- >   case maybeUser of
-- >     Just user -> do
-- >       whenFalse [PasswordIncorrect] -< password user == loginPassword payload
-- >       returnA -< user
-- >     Nothing -> failure -< ()
--
-- >>> checkT validateLogin (LoginPayload "incorrect_username" "incorrect_password")
-- Left [UserNotFound]
--
-- >>> checkT validateLogin (LoginPayload "correct_username" "incorrect_password")
-- Left [PasswordIncorrect]
--
-- >>> checkT validateLogin (LoginPayload "correct_username" "correct_password")
-- Right (User "correct_username" "correct_password" "user@email.com")

module Data.Check
  ( CheckT
  , Check
  , runCheckT
  , runCheck
  , liftM
  , expectM
  , expect
  , expectAll
  , supposeM
  , suppose
  , whenFalse
  , err
  , failure
  ) where

import Control.Arrow
import Control.Category
import Control.Monad.IO.Class
import Data.Functor
import Data.Functor.Identity
import Data.Profunctor
import Data.Semigroup
import Prelude hiding (id, (.))

-- | 'CheckT' transforms an @input@ into an @output@ inside some @m@ while potentially generating @error@s
newtype CheckT m error input output =
  CheckT { unCheckT :: input -> m (Maybe error,output) }

-- | Non-effectful checking
type Check error input output = CheckT Identity error input output

-- | Get the results of running an effectful check on an input
runCheckT :: Monad m => CheckT m e i o -> i -> m (Either e o)
runCheckT ab a = do
  (errs,b) <- unCheckT ab a
  return $ maybe (Right b) Left errs

-- | Get the results of running a non-effectful check on an input
runCheck :: Check e i o -> i -> Either e o
runCheck c a = runIdentity $ runCheckT c a

instance (Monad m, Semigroup e) => Category (CheckT m e) where
  id = CheckT (pure . (,) Nothing)
  bc . ab = CheckT $ \a -> do
    (errs, b) <- unCheckT ab a
    (errs', c) <- unCheckT bc b
    return (errs <> errs', c)

instance Functor m => Functor (CheckT m e i) where
  fmap f ab = CheckT (fmap (fmap f) . unCheckT ab)

instance (Applicative m, Semigroup e) => Applicative (CheckT m e i) where
  pure b = CheckT (pure . (,) Nothing . const b)
  (CheckT f) <*> (CheckT b) = CheckT $ \a -> combine <$> f a <*> b a
    where
      combine f' b' = (fst f' <> fst b', snd f' $ snd b')

instance Functor m => Profunctor (CheckT m e) where
  lmap f ab = CheckT (unCheckT ab . f)
  rmap = fmap

instance (Monad m, Semigroup e) => Arrow (CheckT m e) where
  arr f = CheckT (pure . (,) Nothing . f)
  ab *** ab' = CheckT $ \(a,a') -> do
    (errs,b) <- unCheckT ab a
    (errs',b') <- unCheckT ab' a'
    return (errs <> errs',(b,b'))

-- | Lift an effectful function into a 'CheckT' computation
liftM :: Functor m => (i -> m o) -> CheckT m e i o
liftM f = CheckT (fmap ((,) Nothing) . f)

instance (Monad m, Semigroup e) => ArrowChoice (CheckT m e) where
  ab +++ ab' = CheckT $ \ea -> case ea of
    Left a -> do
      b <- unCheckT ab a
      return $ Left <$> b
    Right a' -> do
      b' <- unCheckT ab' a'
      return $ Right <$> b'

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
expectM p e = CheckT $ \a -> do
  res <- p a
  return (if res then Nothing else Just e, a)

-- | Like 'expectM' but with a non-effectful predicate
expect :: (Applicative m, Semigroup e) => (i -> Bool) -> e -> CheckT m e i i
expect p e = CheckT $ \a -> pure (if p a then Nothing else Just e,a)

-- | Combine many predicate-error pairs into a single check
--
-- > expectAll = foldMap (uncurry expectM)
expectAll :: (Foldable f, Monad m, Semigroup e) => f (i -> m Bool, e) -> CheckT m e i i
expectAll = foldMap (uncurry expectM)

-- | Runs a predicate, logging some errors if the predicate fails
--
-- Returns the result of the predicate
supposeM :: (Monad m, Semigroup e) => (i -> m Bool) -> e -> CheckT m e i Bool
supposeM p e = CheckT $ \a -> do
  res <- p a
  return (if res then Nothing else Just e,res)

-- | Like 'supposeM' but with a non-effectful predicate
suppose :: (Applicative m, Semigroup e) => (i -> Bool) -> e -> CheckT m e i Bool
suppose p e = CheckT $ \a -> let res = p a in pure (if res then Nothing else Just e,res)

-- | Logs an error when `False` is passed in
whenFalse :: (Applicative m, Semigroup e) => e -> CheckT m e Bool Bool
whenFalse = expect id

-- | Fail with the specified error(s)
err :: Applicative m => e -> CheckT m e i o
err e = CheckT (pure . const (Just e,undefined))

-- | A check that will fail on any input
--
-- > failure = err mempty
failure :: (Applicative m, Monoid e) => CheckT m e i o
failure = err mempty
