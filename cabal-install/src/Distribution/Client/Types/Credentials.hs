module Distribution.Client.Types.Credentials (
    Username (..),
    Password (..),
    Token (..)
) where

import Prelude (String)

newtype Username = Username { unUsername :: String }
newtype Password = Password { unPassword :: String }
newtype Token = Token { unToken :: String }
