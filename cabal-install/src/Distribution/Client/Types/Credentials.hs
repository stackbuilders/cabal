module Distribution.Client.Types.Credentials (
    Auth (..),
    Username (..),
    Password (..),
    Token (..),
    unAuthUsername,
    unAuthPassword,
    unAuthToken
) where

import Prelude (String, Maybe (..))

data Auth = AuthCredentials Username Password | AuthToken Token

newtype Username = Username { unUsername :: String }
newtype Password = Password { unPassword :: String }
newtype Token = Token { unToken :: String }

unAuthUsername :: Auth -> Maybe Username
unAuthUsername (AuthCredentials username _) = Just username
unAuthUsername _ = Nothing

unAuthPassword :: Auth -> Maybe Password
unAuthPassword (AuthCredentials _ password) = Just password
unAuthPassword _ = Nothing

unAuthToken :: Auth -> Maybe Token
unAuthToken (AuthToken token) = Just token
unAuthToken _ = Nothing 
