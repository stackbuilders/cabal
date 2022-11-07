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

import Prelude (Maybe(..), String)

data Auth
  = AuthCredentials Credentials
  | AuthToken Token

data Credentials
  = Credentials
      { credentialsUsername :: Username
      , credentialsPassword :: Password
      }

newtype Username = Username { unUsername :: String }
newtype Password = Password { unPassword :: String }
newtype Token = Token { unToken :: String }

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
