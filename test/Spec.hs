{-# language Arrows #-}
{-# language OverloadedStrings #-}
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Arrow
import Control.Applicative
import Control.Monad.IO.Class
import Data.Char
import Data.Monoid
import Data.List
import Data.Profunctor
import Data.These

import Data.Check (CheckT)
import qualified Data.Check as Check

data CharError = NotAscii | NotLatin1 | NotDigit
  deriving (Eq, Show)

prop_first :: Property
prop_first =
  property $ do
    let
      f = proc input -> do
        Check.expect isAscii [NotAscii] -< input
        Check.expect isLatin1 [NotLatin1] -< input
        returnA -< input
      g = proc input -> do
        Check.expect isDigit [NotDigit] -< input
        returnA -< input
    input <- forAll $ liftA2 (,) Gen.unicodeAll Gen.unicodeAll
    Check.runCheck (first (f >>> g)) input === Check.runCheck (first f >>> first g) input

prop_left :: Property
prop_left =
  property $ do
    let
      f = proc input -> do
        Check.expect isAscii [NotAscii] -< input
        Check.expect isLatin1 [NotLatin1] -< input
        returnA -< input
      g = proc input -> do
        Check.expect isDigit [NotDigit] -< input
        returnA -< input
    input <-
      forAll $ Gen.choice [Left <$> Gen.unicodeAll, Right <$> Gen.unicodeAll]
    Check.runCheck (left (f >>> g)) input === Check.runCheck (left f >>> left g) input



data User
  = User
  { username :: String
  , password :: String
  , email :: String
  }
  deriving (Eq, Show)

data UserError
  = EmailInvalid
  | UsernameNotAllowed
  | PasswordTooWeak
  deriving (Eq, Show)

validEmail = not . isPrefixOf "invalid"
strongPassword = (>= 8) . length

database =
  [ User "foo" "somepassword" "foo@gmail.com"
  , User "bar" "hunter2" "bar@gmail.com"
  , User "baz" "pokemonXOXO" "baz@gmail.com"
  ]

usernameAllowed :: String -> IO Bool
usernameAllowed u =
  pure $ u `notElem` fmap username database

validateUser :: CheckT IO [UserError] User User
validateUser =
  Check.expectM (usernameAllowed . username) [UsernameNotAllowed] >>>
  Check.expect (strongPassword . password) [PasswordTooWeak] >>>
  Check.expect (validEmail . email) [EmailInvalid]

validateUser_monad :: CheckT IO [UserError] User User
validateUser_monad = do
  Check.expectM (usernameAllowed . username) [UsernameNotAllowed]
  Check.expect (strongPassword . password) [PasswordTooWeak]
  Check.expect (validEmail . email) [EmailInvalid]

prop_example_1_fail :: Property
prop_example_1_fail =
  property $ do
    u <- forAll $
      Gen.filter
        (`notElem` fmap username database)
        (Gen.string (Range.constant 0 100) Gen.ascii)
    p <- forAll $ Gen.string (Range.constant 0 7) Gen.ascii
    e <- forAll $ ("invalid" <>) <$> Gen.string (Range.constant 0 100) Gen.ascii
    res <- liftIO $ Check.runCheckT validateUser (User u p e)
    res === These [PasswordTooWeak, EmailInvalid] (User u p e)

prop_example_1_success :: Property
prop_example_1_success =
  property $ do
    u <- forAll $
      Gen.filter
        (`notElem` fmap username database)
        (Gen.string (Range.constant 0 100) Gen.ascii)
    p <- forAll $ Gen.string (Range.constant 8 100) Gen.ascii
    e <- forAll .
      Gen.filter validEmail $ Gen.string (Range.constant 0 100) Gen.ascii
    res <- liftIO $ Check.runCheckT validateUser (User u p e)
    res === That (User u p e)

prop_example_1_fail_monad :: Property
prop_example_1_fail_monad =
  property $ do
    u <- forAll $
      Gen.filter
        (`notElem` fmap username database)
        (Gen.string (Range.constant 0 100) Gen.ascii)
    p <- forAll $ Gen.string (Range.constant 0 7) Gen.ascii
    e <- forAll $ ("invalid" <>) <$> Gen.string (Range.constant 0 100) Gen.ascii
    res <- liftIO $ Check.runCheckT validateUser_monad (User u p e)
    res === These [PasswordTooWeak, EmailInvalid] (User u p e)

prop_example_1_success_monad :: Property
prop_example_1_success_monad =
  property $ do
    u <- forAll $
      Gen.filter
        (`notElem` fmap username database)
        (Gen.string (Range.constant 0 100) Gen.ascii)
    p <- forAll $ Gen.string (Range.constant 8 100) Gen.ascii
    e <- forAll .
      Gen.filter validEmail $ Gen.string (Range.constant 0 100) Gen.ascii
    res <- liftIO $ Check.runCheckT validateUser_monad (User u p e)
    res === That (User u p e)

data LoginPayload
  = LoginPayload
  { loginUsername :: String
  , loginPassword :: String
  }
  deriving (Eq, Show)

data LoginError = UserNotFound | PasswordIncorrect
  deriving (Eq, Show)

lookupUser :: String -> IO (Maybe User)
lookupUser name = pure $ findIn database
  where
    findIn [] = Nothing
    findIn (u@User{ username = name'} : us)
      | name' == name = Just u
      | otherwise = findIn us

validateLogin :: CheckT IO [LoginError] LoginPayload User
validateLogin = proc payload -> do
  maybeUser <- Check.liftEffect lookupUser -< loginUsername payload
  case maybeUser of
    Just user -> do
      Check.whenFalse [PasswordIncorrect] -< password user == loginPassword payload
      returnA -< user
    Nothing -> Check.fatal [UserNotFound] -< ()

validateLogin_monad :: CheckT IO [LoginError] LoginPayload User
validateLogin_monad = do
  maybeUser <- lmap loginUsername $ Check.liftEffect lookupUser
  case maybeUser of
    Just user -> do
      lmap ((password user ==) . loginPassword) $ Check.whenFalse [PasswordIncorrect]
      pure user
    Nothing -> Check.fatal [UserNotFound]

prop_example_2_fail_1 :: Property
prop_example_2_fail_1 =
  property $ do
    u <- forAll $
      Gen.filter
        (`notElem` fmap username database)
        (Gen.string (Range.constant 0 100) Gen.ascii)
    p <- forAll $ Gen.string (Range.constant 0 100) Gen.ascii
    res <- liftIO $ Check.runCheckT validateLogin (LoginPayload u p)
    res === This [UserNotFound]

prop_example_2_fail_2 :: Property
prop_example_2_fail_2 =
  property $ do
    u <- forAll $ Gen.element database
    p <- forAll $
      Gen.filter
        (/= password u)
        (Gen.string (Range.constant 0 100) Gen.ascii)
    res <- liftIO $ Check.runCheckT validateLogin (LoginPayload (username u) p)
    res === These [PasswordIncorrect] u

prop_example_2_success :: Property
prop_example_2_success =
  property $ do
    u <- forAll $ Gen.element database
    res <- liftIO $
      Check.runCheckT validateLogin (LoginPayload (username u) $ password u)
    res === That u

prop_example_2_fail_1_monad :: Property
prop_example_2_fail_1_monad =
  property $ do
    u <- forAll $
      Gen.filter
        (`notElem` fmap username database)
        (Gen.string (Range.constant 0 100) Gen.ascii)
    p <- forAll $ Gen.string (Range.constant 0 100) Gen.ascii
    res <- liftIO $ Check.runCheckT validateLogin_monad (LoginPayload u p)
    res === This [UserNotFound]

prop_example_2_fail_2_monad :: Property
prop_example_2_fail_2_monad =
  property $ do
    u <- forAll $ Gen.element database
    p <- forAll $
      Gen.filter
        (/= password u)
        (Gen.string (Range.constant 0 100) Gen.ascii)
    res <- liftIO $ Check.runCheckT validateLogin_monad (LoginPayload (username u) p)
    res === These [PasswordIncorrect] u

prop_example_2_success_monad :: Property
prop_example_2_success_monad =
  property $ do
    u <- forAll $ Gen.element database
    res <- liftIO $
      Check.runCheckT validateLogin_monad (LoginPayload (username u) $ password u)
    res === That u

main :: IO Bool
main = do
  checkSequential
    Group
    { groupName = "Laws"
    , groupProperties =
      [ ("first (f >>> g) = first f >>> first g", prop_first)
      , ("left (f >>> g) = left f >>> left g", prop_left)
      ]
    }

  checkSequential
    Group
    { groupName = "Example - Arrow"
    , groupProperties =
      [ ("Example 1 failure", prop_example_1_fail)
      , ("Example 1 success", prop_example_1_success)
      , ("Example 2 failure 1", prop_example_2_fail_1)
      , ("Example 2 failure 2", prop_example_2_fail_2)
      , ("Example 2 success", prop_example_2_success)
      ]
    }

  checkSequential
    Group
    { groupName = "Example - Monad"
    , groupProperties =
      [ ("Example 1 failure", prop_example_1_fail_monad)
      , ("Example 1 success", prop_example_1_success_monad)
      , ("Example 2 failure 1", prop_example_2_fail_1_monad)
      , ("Example 2 failure 2", prop_example_2_fail_2_monad)
      , ("Example 2 success", prop_example_2_success_monad)
      ]
    }
