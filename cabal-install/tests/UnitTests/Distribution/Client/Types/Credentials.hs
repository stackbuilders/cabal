module UnitTests.Distribution.Client.Types.Credentials
  ( tests
  ) where

import Distribution.Client.Types.Credentials
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests = pure $ testGroup "Crendentials"
  [ 
    testGroup "mkCredentials"
      [ testCase "creates credentials" $
          let credentials = ("username", "password")
              credentialsExpectation = (Credentials (Username "username") (Password "password"))
          in (mkCredentials credentials) @?= credentialsExpectation
      ],
    testGroup "unAuthCredentials"
      [ testCase "unwraps credentials" $
          let auth = (AuthCredentials (Credentials (Username "username") (Password "password")))
              credentialsExpectation = (Just (Credentials (Username "username") (Password "password")))
          in (unAuthCredentials auth) @?= credentialsExpectation
      , testCase "returns nothing when unwraping token" $
          let auth = (AuthToken (Token "token"))
              credentialsExpectation = Nothing
          in (unAuthCredentials auth) @?= credentialsExpectation
      ],
    testGroup "unAuthToken"
      [ testCase "unwraps token" $
          let auth = (AuthToken (Token "token"))
              credentialsExpectation = (Just (Token "token"))
          in (unAuthToken auth) @?= credentialsExpectation
      , testCase "returns nothing when unwraping credentials" $
          let auth = (AuthCredentials (Credentials (Username "username") (Password "password")))
              credentialsExpectation = Nothing
          in (unAuthToken auth) @?= credentialsExpectation
      ],
    testGroup "unCredentials"
      [ testCase "returns username and password" $
          let credentials = (Credentials (Username "username") (Password "password"))
              expectedPair = ("username", "password")
          in (unCredentials credentials) @?= expectedPair
      ]
  ]
