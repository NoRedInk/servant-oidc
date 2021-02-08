-- |
-- Description : Discovery endpoint of a OpenIDClient provider.
module OpenIdConnect.Discovery where

import "base" Control.Monad.Fail (fail)
import "aeson" Data.Aeson
  ( FromJSON (parseJSON),
    Object,
    ToJSON (toJSON),
    Value,
    object,
    withObject,
    withText,
    (.:),
    (.=),
  )
import "aeson" Data.Aeson.Types (Parser)
import "base" Data.Semigroup ((<>))
import "text" Data.Text (Text, unpack)
import "base" GHC.Generics (Generic)
import "network-uri" Network.URI (URI, parseURI)
import "this" OAuth2.Authorize (ResponseType)
import "servant" Servant.API (Get, JSON, (:>))

type API =
  ".well-known"
    :> "openid-configuration"
    :> Get '[JSON] Response

data Response = Response
  { issuer :: URI,
    authorizationEndpoint :: URI,
    tokenEndpoint :: URI,
    userinfoEndpoint :: URI,
    jwksUri :: URI,
    scopesSupported :: [Text],
    responseTypesSupported :: [ResponseType],
    responseModesSupported :: [ResponseMode],
    tokenEndpointAuthMethodsSupported :: [TokenEndpointAuthMethod],
    subjectTypesSupported :: [SubjectType],
    claimTypesSupported :: [ClaimType],
    claimsSupported :: [Claim],
    idTokenSigningAlgValuesSupported :: [IdTokenSigningAlgValue]
  }
  deriving (Show, Generic)

instance FromJSON Response where
  parseJSON =
    withObject "Response" $ \v -> do
      issuer <- parseURI' v "issuer"
      authorizationEndpoint <- parseURI' v "authorization_endpoint"
      tokenEndpoint <- parseURI' v "token_endpoint"
      userinfoEndpoint <- parseURI' v "userinfo_endpoint"
      jwksUri <- parseURI' v "jwks_uri"
      scopesSupported <- v .: "scopes_supported"
      responseTypesSupported <- v .: "response_types_supported"
      responseModesSupported <- v .: "response_modes_supported"
      tokenEndpointAuthMethodsSupported <-
        v .: "token_endpoint_auth_methods_supported"
      subjectTypesSupported <- v .: "subject_types_supported"
      claimTypesSupported <- v .: "claim_types_supported"
      claimsSupported <- v .: "claims_supported"
      idTokenSigningAlgValuesSupported <-
        v .: "id_token_signing_alg_values_supported"
      pure
        Response
          { issuer,
            authorizationEndpoint,
            tokenEndpoint,
            userinfoEndpoint,
            jwksUri,
            scopesSupported,
            responseTypesSupported,
            responseModesSupported,
            tokenEndpointAuthMethodsSupported,
            subjectTypesSupported,
            claimTypesSupported,
            claimsSupported,
            idTokenSigningAlgValuesSupported
          }
    where
      parseURI' :: Object -> Text -> Parser URI
      parseURI' obj x = do
        str <- obj .: x
        case parseURI str of
          Just uri -> pure uri
          Nothing ->
            Control.Monad.Fail.fail
              (unpack $ "Could not parse URI for field `" <> x <> "`")

instance ToJSON Response where
  toJSON response =
    object
      [ fromURI "issuer" (issuer response),
        fromURI "authorization_endpoint" (authorizationEndpoint response),
        fromURI "token_endpoint" (tokenEndpoint response),
        fromURI "userinfo_endpoint" (userinfoEndpoint response),
        fromURI "jwks_uri" (jwksUri response),
        "scopes_supported" .= scopesSupported response,
        "response_types_supported" .= responseTypesSupported response,
        "response_modes_supported" .= responseModesSupported response,
        "token_endpoint_auth_methods_supported"
          .= tokenEndpointAuthMethodsSupported response,
        "subject_types_supported" .= subjectTypesSupported response,
        "claim_types_supported" .= claimTypesSupported response,
        "claims_supported" .= claimsSupported response,
        "id_token_signing_alg_values_supported"
          .= idTokenSigningAlgValuesSupported response
      ]
    where
      fromURI :: Text -> URI -> (Text, Value)
      fromURI x uri = (x, toJSON $ show uri)

data ResponseMode
  = Query
  deriving (Show)

instance FromJSON ResponseMode where
  parseJSON =
    withText "ResponseMode" $ \case
      "query" -> pure Query
      _ -> Control.Monad.Fail.fail "Unknown response mode"

instance ToJSON ResponseMode where
  toJSON Query = "query"

data TokenEndpointAuthMethod
  = ClientSecretBasic
  deriving (Show)

instance FromJSON TokenEndpointAuthMethod where
  parseJSON =
    withText "TokenEndpointAuthMethod" $ \case
      "client_secret_basic" -> pure ClientSecretBasic
      _ -> Control.Monad.Fail.fail "Unknown token endpoint auth method"

instance ToJSON TokenEndpointAuthMethod where
  toJSON ClientSecretBasic = "client_secret_basic"

data SubjectType
  = Public
  deriving (Show)

instance FromJSON SubjectType where
  parseJSON =
    withText "SubjectType" $ \case
      "public" -> pure Public
      _ -> Control.Monad.Fail.fail "Unknown subject type"

instance ToJSON SubjectType where
  toJSON Public = "public"

data ClaimType
  = Normal
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

data IdTokenSigningAlgValue
  = RS256
  deriving (Show)

instance FromJSON IdTokenSigningAlgValue where
  parseJSON =
    withText "IdTokenSigningAlgValue" $ \case
      "RS256" -> pure RS256
      _ -> Control.Monad.Fail.fail "Unknown signing algorithm"

instance ToJSON IdTokenSigningAlgValue where
  toJSON RS256 = "RS256"
