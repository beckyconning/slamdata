module OIDC.Aff where

import Prelude
import Debug.Trace (traceAny)
import Global as Global
import OIDCCryptUtils as Cryptography
import Data.String as String
import Control.Monad.Eff (Eff())
import DOM (DOM())
import Data.Argonaut.Core (Json())
import Control.UI.Browser as Browser

type URI = String

requestAuthenticationURI
  :: URI
  -> Cryptography.ClientID
  -> URI
  -> Cryptography.BoundStateJWS
  -> Cryptography.HashedNonce
  -> URI
requestAuthenticationURI authenticationURI clientId redirectURI state nonce =
  uriWithParameters
    authenticationURI
    [ uriComponent "response_type" "id_token token"
    , uriComponent "client_id" $ Cryptography.runClientID clientId
    , uriComponent "redirect_uri" redirectURI
    , uriComponent "scope" "openid email"
    , uriComponent "state" $ Cryptography.runBoundStateJWS state
    , uriComponent "nonce" $ Cryptography.runHashedNonce nonce
    ]

uriComponent :: String -> String -> String
uriComponent key value = key ++ "=" ++ (Global.encodeURIComponent value)

uriWithParameters :: String -> Array String -> String
uriWithParameters base xs = base ++ "?" ++ (String.joinWith "&" xs)

exampleClientId :: Cryptography.ClientID
exampleClientId = Cryptography.ClientID "945479264233-kumsko0q3e5eh3efd8pll1da9kua3i3b.apps.googleusercontent.com"

exampleRedirectURI :: URI
exampleRedirectURI = "http://localhost:8080/files/auth_redirect.html/"

exampleNonce :: Cryptography.HashedNonce
exampleNonce = Cryptography.hashNonce $ Cryptography.UnhashedNonce "randomReplay"

exampleUnboundState :: Cryptography.StateString
exampleUnboundState = Cryptography.StateString "http://localhost:8080/files/index.html#?q=path%3A%22%2Ftest-mount%2F%22&sort=asc&salt=1311046"

exampleBoundState :: Cryptography.BoundStateJWS
exampleBoundState = Cryptography.bindState exampleUnboundState $ Cryptography.KeyString "randomCSRF"

exampleAuthURI :: URI
exampleAuthURI = "https://accounts.google.com/o/oauth2/v2/auth"

exampleURI :: URI
exampleURI =
  requestAuthenticationURI
    exampleAuthURI
    exampleClientId
    exampleRedirectURI
    exampleBoundState
    exampleNonce

requestAuthentication :: forall e. Eff (dom :: DOM | e) Unit
requestAuthentication = Browser.setLocation exampleURI

