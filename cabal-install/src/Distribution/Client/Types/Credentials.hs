module Distribution.Client.Types.Credentials (
    Auth (..),
    Credentials (..),
    Username (..),
    Password (..),
    Token (..)
) where

import Prelude (String)

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
