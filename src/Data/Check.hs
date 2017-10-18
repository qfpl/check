{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Check is used to "check" inputs and accumulate errors on failure
--
-- This is done by building a `CheckT` computation, which contains all the rules that some data
-- should satisfy, then using `checkT` to run it with some input. If errors occurred
-- they are accumulated and returned, and an output may also be returned if those errors
-- were not considered "fatal".
--
-- Here's a simple example:
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
--
-- `CheckT` is a 'Category', so for simple cases data can be validated by
-- composing your expectations using `(>>>)`:
--
-- > validateUser :: CheckT IO [UserError] User User
-- > validateUser =
-- >   expectM (usernameAllowed . username) [UsernameNotAllowed] >>>
-- >   expect (strongPassword . password) [PasswordTooWeak] >>>
-- >   expect (validEmail . email) [EmailInvalid]
--
-- `expectM`, `expect` and `expectAll` are used test predicates against the input
-- data, and log errors if the predicates don't hold. Unlike the [Validation type](https://hackage.haskell.org/package/validation), the result is not forgotten
-- in the presence of errors, so you can decompose your validation logic
-- into smaller more manageable components.
--
-- You can also build a `CheckT` using a monadic interface. For simple cases,
-- the translation from Arrow syntax is nearly identical. Here's the above
-- example re-written using do-notation.
--
-- > validateUser :: CheckT IO [UserError] User User
-- > validateUser = do
-- >   expectM (usernameAllowed . username) [UsernameNotAllowed]
-- >   expect (strongPassword . password) [PasswordTooWeak]
-- >   expect (validEmail . email) [EmailInvalid]
--
-- They're the same program.
--
-- >>> checkT validateUser (User "allowed_username" "weak" "invalid@email.com")
-- These [PasswordTooWeak, EmailInvalid] (User "allowed_username" "weak" "invalid@email.com")
--
-- >>> checkT validateUser (User "allowed_username" "strong_password" "valid@email.com")
-- That (User "allowed_username" "strong_password" "valid_email")
--
-- A `CheckT` computation can have an output type that is different to its input type,
-- as well as letting intermediate results influence the flow of checking:
--
-- > data LoginPayload = LoginPayload { loginUsername :: String, loginPassword :: String }
-- >
-- > data LoginError = UserNotFound PasswordIncorrect
-- >
-- > lookupUser :: String -> IO (Maybe User)
-- > lookupUser name = ...
--
-- In these cases, it's often easier to use Arrow notation:
--
-- > validateLogin :: CheckT IO [LoginError] LoginPayload User
-- > validateLogin = proc payload -> do
-- >   maybeUser <- liftEffect lookupUser -< loginUsername payload
-- >   case maybeUser of
-- >     Just user -> do
-- >       whenFalse [PasswordIncorrect] -< password user == loginPassword payload
-- >       returnA -< user
-- >     Nothing -> fatal [UserNotFound] -< ()
--
-- `liftEffect` applies an effectful transformation to the input, doing no
-- checking in the process. In this case, it "does a database lookup" using
-- some login information.
--
-- `whenFalse` will log some errors when its input is `False`. This is similar
-- to an assert statement- it doesn't alter the flow of the program when the
-- input is `True`
--
-- `fatal` logs a single, fatal set of errors, meaning that only the errors
-- passed to `fatal` will be output if it is reached. This is useful when
-- you can't produce the required output type. In the above example, if a user
-- is not in the database then it is "impossible" to produce a value of type
-- @User@. Thus, we fail with a `fatal` error.
--
-- To translate this example to do-notation, we need to use `lmap` from
-- `Data.Profunctor` in order to refine the input to the `CheckT`. Here's what that
-- looks like:
--
-- > validateLogin :: CheckT IO [LoginError] LoginPayload User
-- > validateLogin = do
-- >   maybeUser <- lmap loginUsername $ Check.liftEffect lookupUser
-- >     case maybeUser of
-- >       Just user -> do
-- >         lmap ((password user ==) . loginPassword) $ Check.whenFalse [PasswordIncorrect]
-- >         pure user
-- >       Nothing -> Check.fatal [UserNotFound]
--
-- Again, they are the same program
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
  , fatal
  , failure
  ) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad.Chronicle
import Control.Monad.Reader
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

instance (Monad m, Semigroup e) => MonadReader i (CheckT m e i) where
  ask = id
  local = lmap

instance (MonadIO m, Semigroup e) => MonadIO (CheckT m e i) where
  liftIO = liftEffect . const . liftIO 

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
-- Returns its input
--
-- > whenFalse = expect id
whenFalse :: (Monad m, Semigroup e) => e -> CheckT m e Bool Bool
whenFalse = expect id

-- | Fail with the specified error(s), producing no output
fatal :: (Monad m, Semigroup e) => e -> CheckT m e i o
fatal = CheckT . Kleisli . const . confess

-- | Fail on any input, producing no output
--
-- > failure = fatal mempty
failure :: (Monad m, Semigroup e, Monoid e) => CheckT m e i o
failure = fatal mempty
