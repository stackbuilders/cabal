module UnitTests.Distribution.Client.HttpUtils
  ( tests
  ) where

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
      ]
  ]

testPostHttpFileByProgram :: String -> TestTree
testPostHttpFileByProgram program = testGroup program
  [ testCase "credentials" $ foo "curl"
      (AuthToken $ Token "foo")
      "Error: Bad auth token\n"
  , testCase "token" $ foo "curl"
      (AuthCredentials $ Credentials (Username "foo") (Password "bar"))
      "Error: Username or password incorrect\n"
  ]

foo :: String -> Auth -> String -> IO ()
foo program auth message = do
  let uri = fromJust $ parseURI "https://hackage.haskell.org/packages/candidates"
  transport <- configureTransport silent [] (Just program)
  response <- postHttpFile transport silent uri "tests/fixtures/files/fake.tar.gz" (Just auth)
  response @=? (401, message)
