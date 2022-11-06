module UnitTests.Distribution.Client.HttpUtils
  ( tests
  ) where

import Data.Maybe (fromJust)
import Distribution.Client.HttpUtils (HttpTransport(..), configureTransport)
import Distribution.Client.Types.Credentials
import Distribution.Verbosity (silent, verbose)
import Network.URI (parseURI)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "HttpUtils"
  [ testGroup "postHttpFile"
      [ testCase "curl" testPostHttpFileCurl
      , testCase "curl" testPostHttpFileCurl2
      ]
  ]

testPostHttpFileCurl :: IO ()
testPostHttpFileCurl = do
  let uri = fromJust $ parseURI "https://hackage.haskell.org/packages/candidates"
      auth = AuthToken $ Token "foo"
  transport <- configureTransport silent [] (Just "curl")
  (code, body) <- postHttpFile transport verbose uri "tests/fixtures/files/fake.tar.gz" (Just auth)
  code @=? 401
  body @=? "Error: Bad auth token\n"

testPostHttpFileCurl2 :: IO ()
testPostHttpFileCurl2 = do
  let uri = fromJust $ parseURI "https://hackage.haskell.org/packages/candidates"
      auth = AuthCredentials $ Credentials (Username "foo") (Password "bar")
  transport <- configureTransport silent [] (Just "curl")
  (code, body) <- postHttpFile transport verbose uri "tests/fixtures/files/fake.tar.gz" (Just auth)
  code @=? 401
  body @=? "Error: Username or password incorrect\n"
