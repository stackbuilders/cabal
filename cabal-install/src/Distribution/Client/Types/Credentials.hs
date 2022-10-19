module Distribution.Client.Types.Credentials (
    Auth (..),
    Credentials (..),
    Username (..),
    Password (..),
    Token (..),
    unAuthCredentials,
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

unAuthCredentials :: Auth -> Maybe Credentials
unAuthCredentials (AuthCredentials c) = Just c
unAuthCredentials _ = Nothing

unCredentials :: Credentials -> (String, String)
unCredentials (Credentials (Username u) (Password p)) = (u, p)
