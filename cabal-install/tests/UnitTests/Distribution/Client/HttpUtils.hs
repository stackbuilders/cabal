{-# LANGUAGE ScopedTypeVariables #-}

module UnitTests.Distribution.Client.HttpUtils
  ( tests
  ) where

import Control.Exception (SomeException, displayException, try)
import Data.List (isInfixOf)
import Data.Maybe (fromJust)
import Distribution.Client.HttpUtils (HttpTransport(..), configureTransport)
import Distribution.Client.Types.Credentials
import Distribution.Verbosity (silent)
import Network.URI (parseURI)
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests = pure $ testGroup "HttpUtils"
  [ testGroup "postHttpFile"
      [ postHttpFileTests "curl"
          "Username or password incorrect"
          "Bad auth token"
      , postHttpFileTests "wget"
          "Username/Password Authentication Failed"
          "Username/Password Authentication Failed"
      , postHttpFileTests "powershell"
          "Username or password incorrect"
          "Bad auth token"
      , postHttpFileTests "plain-http"
          "Bad Request"
          "Bad auth token"
      ]
  , testGroup "putHttpFile"
      [ putHttpFileTests "curl"
          "\"Authorization\": \"X-ApiKey foo\""
     , putHttpFileTests "wget"
         "\"Authorization\": \"X-ApiKey foo\""
     , putHttpFileTests "powershell"
         "\"Authorization\": \"X-ApiKey foo\""
     ]
  ]

postHttpFileTests :: String -> String -> String -> TestTree
postHttpFileTests program credentialsMessage tokenMessage =
  testGroup program
    [ testCase "credentials" $ testPostHttpFile program
        (AuthCredentials $ Credentials (Username "foo") (Password "bar"))
        credentialsMessage
    , testCase "token" $ testPostHttpFile program
        (AuthToken $ Token "foo")
        tokenMessage
    ]
 
putHttpFileTests :: String -> String -> TestTree
putHttpFileTests program tokenMessage =
  testGroup program
    [ testCase "token" $ testPutHttpFile program
        (AuthToken $ Token "foo")
        tokenMessage
    ]

testPostHttpFile :: String -> Auth -> String -> IO ()
testPostHttpFile program auth message = do
  let uri = fromJust $ parseURI "http://hackage.haskell.org/packages/candidates"
  transport <- configureTransport silent [] (Just program)
  response <- try $ postHttpFile transport silent uri "tests/fixtures/files/test-upload.tar.gz" (Just auth)
  case response of
    (Left (e :: SomeException)) ->
      let body = displayException e
      in isInfixOf message body @? errMessage body
    (Right (_, body)) -> isInfixOf message body @? errMessage body
  where
    errMessage body = "Expect \"" ++ body ++ "\" to contain \""  ++ message ++ "\""

testPutHttpFile :: String -> Auth -> String -> IO ()
testPutHttpFile program auth message = do
  let uri = fromJust $ parseURI "http://httpbin.org/anything"
  transport <- configureTransport silent [] (Just program)
  response <- try $ postHttpFile transport silent uri "tests/fixtures/files/test-upload.tar.gz" (Just auth)
  case response of
    (Left (e :: SomeException)) ->
      let body = displayException e
      in isInfixOf message body @? errMessage body
    (Right (_, body)) -> isInfixOf message body @? errMessage body
  where
    errMessage body = "Expect \"" ++ body ++ "\" to contain \""  ++ message ++ "\""
