module UnitTests.Distribution.Client.HttpUtils
  ( tests
  ) where

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
      [ testGroup "curl"
          [ testCase "credentials" $ testPostHttpFileCurl
              (AuthToken $ Token "foo")
              "Bad auth token"
          , testCase "token" $ testPostHttpFileCurl
              (AuthCredentials $ Credentials (Username "foo") (Password "bar"))
              "Username or password incorrect"
          ]
      -- , testPostHttpFile "wget"
      -- , testPostHttpFile "powershell"
      , testGroup "plain-http"
          [ testCase "credentials" $ testPostHttpFilePlainHttp
              (AuthToken $ Token "foo")
              (401, "Bad auth token")
          , testCase "token" $ testPostHttpFilePlainHttp
              (AuthCredentials $ Credentials (Username "foo") (Password "bar"))
              (400, "Bad Request")
          ]
      ]
  ]

testPostHttpFileCurl :: Auth -> String -> IO ()
testPostHttpFileCurl auth message = do
  let uri = fromJust $ parseURI "https://hackage.haskell.org/packages/candidates"
  transport <- configureTransport silent [] (Just "curl")
  (code, body) <- postHttpFile transport silent uri "tests/fixtures/files/fake.tar.gz" (Just auth)
  code @?= 401
  isInfixOf message body @? "Expect \"" ++ body ++ "\" to contain \""  ++ message ++ "\""

testPostHttpFilePlainHttp :: Auth -> (Int, String) -> IO ()
testPostHttpFilePlainHttp auth (expectedCode, message) = do
  let uri = fromJust $ parseURI "http://hackage.haskell.org/packages/candidates"
  transport <- configureTransport silent [] (Just "plain-http")
  (code, body) <- postHttpFile transport silent uri "tests/fixtures/files/fake.tar.gz" (Just auth)
  code @?= expectedCode
  isInfixOf message body @? "Expect \"" ++ body ++ "\" to contain \""  ++ message ++ "\""
