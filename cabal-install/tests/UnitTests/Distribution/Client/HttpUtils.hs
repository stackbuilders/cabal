{-# LANGUAGE ScopedTypeVariables #-}

module UnitTests.Distribution.Client.HttpUtils
  ( tests
  ) where

import Control.Exception (SomeException, displayException, try)
import Data.List (isInfixOf)
import Data.Maybe (fromJust)
import Distribution.Client.HttpUtils (HttpTransport(..), configureTransport)
import Distribution.Client.Types.Credentials
import Distribution.Verbosity (silent, verbose)
import Network.URI (parseURI)
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests = pure $ testGroup "HttpUtils"
  [ testGroup "postHttpFile"
      [ postHttpFileTests "curl"
          (401, "Username or password incorrect")
          (401, "Bad auth token")
      , postHttpFileTests "wget"
          (401, "Username/Password Authentication Failed")
          (401, "Username/Password Authentication Failed")
      , postHttpFileTests "powershell"
          (401, "Username or password incorrect")
          (401, "Bad auth token")
      , postHttpFileTests "plain-http"
          (400, "Bad Request")
          (401, "Bad auth token")
      ]
  , testGroup "putHttpFile"
      [ putHttpFileTests "curl"
          (401, "Username or password incorrect")
          (401, "Bad auth token")
      ]
  ]

postHttpFileTests :: String -> (Int, String) -> (Int, String) -> TestTree
postHttpFileTests program credentialsExpectations tokenExpectations =
  testGroup program
    [ testCase "credentials" $ testPostHttpFile program
        (AuthCredentials $ Credentials (Username "foo") (Password "bar"))
        credentialsExpectations
    , testCase "token" $ testPostHttpFile program
        (AuthToken $ Token "foo")
        tokenExpectations
    ]

testPostHttpFile :: String -> Auth -> (Int, String) -> IO ()
testPostHttpFile program auth (expectedCode, message) = do
  let uri = fromJust $ parseURI "http://hackage.haskell.org/packages/candidates"
  transport <- configureTransport silent [] (Just program)
  response <- try $ postHttpFile transport silent uri "tests/fixtures/files/test-upload-0.1.0.0.tar.gz" (Just auth)
  case response of
    (Left (err :: SomeException)) -> do
      let body = displayException err
      isInfixOf message body @? errMessage body
    (Right (code, body)) -> do
      code @?= expectedCode
      isInfixOf message body @? errMessage body
  where
    errMessage body = "Expect \"" ++ body ++ "\" to contain \""  ++ message ++ "\""

putHttpFileTests :: String -> (Int, String) -> (Int, String) -> TestTree
putHttpFileTests program credentialsExpectations tokenExpectations =
  testGroup program
    [ testCase "credentials" $ testPutHttpFile program
        (AuthCredentials $ Credentials (Username "foo") (Password "bar"))
        credentialsExpectations
    , testCase "token" $ testPutHttpFile program
        (AuthToken $ Token "foo")
        tokenExpectations
    ]

testPutHttpFile :: String -> Auth -> (Int, String) -> IO ()
testPutHttpFile program auth (expectedCode, message) = do
  let uri = fromJust $ parseURI "http://hackage.haskell.org/package/cabal-install/candidate/docs"
  transport <- configureTransport silent [] (Just program)
  response <- try $ putHttpFile transport verbose uri "tests/fixtures/files/test-upload-0.1.0.0-docs.tar.gz" (Just auth) []
  case response of
    (Left (err :: SomeException)) -> do
      let body = displayException err
      isInfixOf message body @? errMessage body
    (Right (code, body)) -> do
      code @?= expectedCode
      isInfixOf message body @? errMessage body
  where
    errMessage body = "Expect \"" ++ body ++ "\" to contain \""  ++ message ++ "\""
