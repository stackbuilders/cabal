{-# LANGUAGE ScopedTypeVariables #-}

module UnitTests.Distribution.Client.HttpUtils
  ( tests
  ) where

import Control.Exception
import Data.List (isInfixOf)
import Data.Maybe (fromJust)
import Distribution.Client.HttpUtils (HttpTransport(..), configureTransport)
import Distribution.Client.Types.Credentials
import Distribution.Verbosity (silent)
import Network.URI (parseURI)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "HttpUtils"
  [ testGroup "postHttpFile"
      [ testPostHttpFileByProgram "curl"
          (401, "Username or password incorrect")
          (401, "Bad auth token")
      , testPostHttpFileByProgram "wget"
          (401, "Username/Password Authentication Failed")
          (401, "Username/Password Authentication Failed")
      , testPostHttpFileByProgram "powershell"
          (401, "")
          (401, "")
      , testPostHttpFileByProgram "plain-http"
          (400, "Bad Request")
          (401, "Bad auth token")
      ]
  ]

testPostHttpFileByProgram :: String -> (Int, String) -> (Int, String) -> TestTree
testPostHttpFileByProgram program credentialsExpectations tokenExpectations =
  testGroup program
    [ testCase "credentials" $ testPostHttpFileByAuth program
        (AuthCredentials $ Credentials (Username "foo") (Password "bar"))
        credentialsExpectations
    , testCase "token" $ testPostHttpFileByAuth program
        (AuthToken $ Token "foo")
        tokenExpectations
    ]

testPostHttpFileByAuth :: String -> Auth -> (Int, String) -> IO ()
testPostHttpFileByAuth program auth (expectedCode, message) = do
  let uri = fromJust $ parseURI "http://hackage.haskell.org/packages/candidates"
  transport <- configureTransport silent [] (Just program)
  eResponse <- try $ postHttpFile transport silent uri "tests/fixtures/files/fake.tar.gz" (Just auth)
  case eResponse of
    (Left (err :: SomeException)) -> do
      let body = displayException err
      isInfixOf message body @? "Expect \"" ++ body ++ "\" to contain \""  ++ message ++ "\""
    (Right (code, body)) -> do
      code @?= expectedCode
      isInfixOf message body @? "Expect \"" ++ body ++ "\" to contain \""  ++ message ++ "\""
