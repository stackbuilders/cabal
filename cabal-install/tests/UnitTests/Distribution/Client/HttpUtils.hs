module UnitTests.Distribution.Client.HttpUtils
  ( tests
  ) where

import Data.Maybe (fromJust)
import Distribution.Client.HttpUtils (HttpTransport(..), configureTransport)
import Distribution.Verbosity (silent)
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
  let uri = fromJust $ parseURI "https://httpbin.org/headers"
  transport <- configureTransport silent [] (Just "curl")
  (code, _) <- postHttpFile transport silent uri "fixture/files/package.tar.gz" Nothing
  code @=? 200
