module Distribution.Client.Types.Credentials (
    Auth (..),
    Username (..),
    Password (..),
    Token (..)
) where

import Prelude (String)

data Auth = AuthCredentials Username Password | AuthToken Token

newtype Username = Username { unUsername :: String }
newtype Password = Password { unPassword :: String }
newtype Token = Token { unToken :: String }
