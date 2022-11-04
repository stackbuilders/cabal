module UnitTests.Distribution.Client.Upload
  ( tests
  ) where

import Distribution.Client.GlobalFlags (RepoContext(..))
import Distribution.Client.Setup (IsCandidate(..))
import Distribution.Client.Types.Repo (Repo(..), emptyRemoteRepo)
import Distribution.Client.Types.RepoName (RepoName(..))
import Distribution.Client.Upload (upload)
import Distribution.Verbosity (silent)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Upload"
  [ testCase "curl" testUploadCurl
  ]

testUploadCurl :: IO ()
testUploadCurl =
  let repo =
        RepoRemote
          { repoRemote = emptyRemoteRepo $ RepoName "fake"
          , repoLocalDir = undefined
          }
      context =
        RepoContext
          { repoContextRepos = [repo]
          , repoContextGetTransport = return undefined
          , repoContextWithSecureRepo = \_ _ -> undefined
          , repoContextIgnoreExpiry = undefined
          }
  in upload silent context Nothing Nothing Nothing IsCandidate []
