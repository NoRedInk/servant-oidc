{-|
Description : Discovery endpoint of a OpenIDClient provider.
-}
module OpenIdConnect.Discovery where

import "base" Control.Monad.Fail (fail)
import "aeson" Data.Aeson
       (FromJSON(parseJSON), ToJSON(toJSON), defaultOptions,
        genericParseJSON, genericToJSON, withText)
import "aeson-casing" Data.Aeson.Casing (snakeCase)
import "aeson" Data.Aeson.Types (fieldLabelModifier)
import "text" Data.Text (Text)
import "base" GHC.Generics (Generic)
import "network-uri" Network.URI (URI)
import "this" OAuth2.Authorize (ResponseType)
import "servant" Servant.API ((:>), Get, JSON)
import "servant-auth-server" Servant.Auth.Server () -- TODO: Get rid of this import (it provides a FromJSON URI orphan instance).

type API
   = ".well-known"
     :> "openid-configuration"
     :> Get '[ JSON] Response

data Response = Response
  { issuer :: URI
  , authorizationEndpoint :: URI
  , tokenEndpoint :: URI
  , userinfoEndpoint :: URI
  , jwksUri :: URI
  , scopesSupported :: [Text]
  , responseTypesSupported :: [ResponseType]
  , responseModesSupported :: [ResponseMode]
  , tokenEndpointAuthMethodsSupported :: [TokenEndpointAuthMethod]
  , subjectTypesSupported :: [SubjectType]
  , claimTypesSupported :: [ClaimType]
  , claimsSupported :: [Claim]
  , idTokenSigningAlgValuesSupported :: [IdTokenSigningAlgValue]
  } deriving (Show, Generic)

instance FromJSON Response where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = snakeCase}

instance ToJSON Response where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = snakeCase}

data ResponseMode =
  Query
  deriving (Show)

instance FromJSON ResponseMode where
  parseJSON =
    withText "ResponseMode" $ \case
      "query" -> pure Query
      _ -> Control.Monad.Fail.fail "Unknown response mode"

instance ToJSON ResponseMode where
  toJSON Query = "query"

data TokenEndpointAuthMethod =
  ClientSecretBasic
  deriving (Show)

instance FromJSON TokenEndpointAuthMethod where
  parseJSON =
    withText "TokenEndpointAuthMethod" $ \case
      "client_secret_basic" -> pure ClientSecretBasic
      _ -> Control.Monad.Fail.fail "Unknown token endpoint auth method"

instance ToJSON TokenEndpointAuthMethod where
  toJSON ClientSecretBasic = "client_secret_basic"

data SubjectType =
  Public
  deriving (Show)

instance FromJSON SubjectType where
  parseJSON =
    withText "SubjectType" $ \case
      "public" -> pure Public
      _ -> Control.Monad.Fail.fail "Unknown subject type"

instance ToJSON SubjectType where
  toJSON Public = "public"

data ClaimType =
  Normal
  deriving (Show)

instance FromJSON ClaimType where
  parseJSON =
    withText "ClaimType" $ \case
      "normal" -> pure Normal
      _ -> Control.Monad.Fail.fail "Unknown claim type"

instance ToJSON ClaimType where
  toJSON Normal = "normal"

data Claim
  = Iss
  | Sub
  | Aud
  | Exp
  | Iat
  | Custom Text
  deriving (Show)

instance FromJSON Claim where
  parseJSON =
    withText "Claim" $ \case
      "iss" -> pure Iss
      "sub" -> pure Sub
      "aud" -> pure Aud
      "exp" -> pure Exp
      "iat" -> pure Iat
      x -> pure $ Custom x

instance ToJSON Claim where
  toJSON Iss = "iss"
  toJSON Sub = "sub"
  toJSON Aud = "aud"
  toJSON Exp = "exp"
  toJSON Iat = "iat"
  toJSON (Custom x) = toJSON x

data IdTokenSigningAlgValue =
  RS256
  deriving (Show)

instance FromJSON IdTokenSigningAlgValue where
  parseJSON =
    withText "IdTokenSigningAlgValue" $ \case
      "RS256" -> pure RS256
      _ -> Control.Monad.Fail.fail "Unknown signing algorithm"

instance ToJSON IdTokenSigningAlgValue where
  toJSON RS256 = "RS256"
