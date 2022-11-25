module Distribution.Client.Types.Credentials (
    Auth (..),
    Credentials (..),
    Username (..),
    Password (..),
    Token (..),
    mkCredentials,
    unAuthCredentials,
    unAuthToken,
    unCredentials
) where

import Prelude (Maybe(..), String, Eq, Show)

data Auth
  = AuthCredentials Credentials
  | AuthToken Token
  deriving (Eq)

data Credentials
  = Credentials
      { credentialsUsername :: Username
      , credentialsPassword :: Password
      }
  deriving (Eq, Show)

newtype Username = Username { unUsername :: String } deriving (Eq, Show)
newtype Password = Password { unPassword :: String } deriving (Eq, Show)
newtype Token = Token { unToken :: String } deriving (Eq, Show)

mkCredentials :: (String, String) -> Credentials
mkCredentials (u, p) = Credentials (Username u) (Password p)

unAuthCredentials :: Auth -> Maybe Credentials
unAuthCredentials (AuthCredentials c) = Just c
unAuthCredentials _ = Nothing

unAuthToken :: Auth -> Maybe Token
unAuthToken (AuthToken t) = Just t
unAuthToken _ = Nothing

unCredentials :: Credentials -> (String, String)
unCredentials (Credentials (Username u) (Password p)) = (u, p)
