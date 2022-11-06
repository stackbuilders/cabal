module UnitTests.Distribution.Client.HttpUtils
  ( tests
  ) where

import Data.Maybe (fromJust)
import Distribution.Client.HttpUtils (HttpTransport(..), configureTransport)
import Distribution.Verbosity (silent, verbose)
import Network.URI (parseURI)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "HttpUtils"
  [ testGroup "postHttpFile"
      [ testCase "curl" testPostHttpFileCurl
      ]
  ]

testPostHttpFileCurl :: IO ()
testPostHttpFileCurl = do
  let uri = fromJust $ parseURI "https://hackage.haskell.org/packages/candidates"
  transport <- configureTransport silent [] (Just "curl")
  (code, body) <- postHttpFile transport verbose uri "tests/fixtures/files/fake.tar.gz" Nothing
  print body
  code @=? 200
