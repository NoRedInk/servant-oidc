{-|
Description : Types shared by OAuth2 modules.
-}
module OAuth2.Types where

import "base" Control.Monad (fail)
import "aeson" Data.Aeson
       (FromJSON(parseJSON), ToJSON(toJSON), withText)
import "cereal" Data.Serialize (Serialize(get, put))
import "base" Data.String (IsString)
import "text" Data.Text (Text, pack, unpack)
import "text" Data.Text.Encoding (decodeUtf8, encodeUtf8)
import "base" GHC.Generics (Generic)
import "network-uri" Network.URI (URI, parseAbsoluteURI)
import "http-api-data" Web.HttpApiData
       (FromHttpApiData(parseUrlPiece), ToHttpApiData(toUrlPiece))

newtype AuthorizationCode =
  AuthorizationCode Text
  deriving (Eq, FromHttpApiData, IsString, ToHttpApiData, Show)

-- | The OAuth2 client identifier
--   This value will be provided for a client app by the OAuth2 provider.
--
--   <https://tools.ietf.org/html/rfc6749#section-2.2>
newtype ClientId =
  ClientId Text
  deriving (Eq, IsString, ToHttpApiData, FromHttpApiData, Show)

-- | The OAuth2 client secret
--   This value will be provided for a client app by the OAuth2 provider.
--
--   <https://tools.ietf.org/html/rfc6749#section-2.3>
newtype ClientSecret =
  ClientSecret Text
  deriving (Eq, IsString, ToHttpApiData, FromHttpApiData, Show)

-- | The URI on a OAuth2 client authorized users should be redirected to.
--
--   <https://tools.ietf.org/html/rfc6749#section-3.1.2>
newtype RedirectURI =
  RedirectURI URI
  deriving (Eq, Show)

instance ToHttpApiData RedirectURI where
  toUrlPiece (RedirectURI uri) = pack $ show uri

instance FromHttpApiData RedirectURI where
  parseUrlPiece =
    maybe (Left "Not a valid absolute URI") Right .
    fmap RedirectURI . parseAbsoluteURI . unpack

-- | An OAuth2 access token.
--
--   <https://tools.ietf.org/html/rfc6749#section-1.4>
newtype AccessToken =
  AccessToken Text
  deriving (Eq, Generic, Show)

instance Serialize AccessToken where
  put (AccessToken txt) = Data.Serialize.put $ encodeUtf8 txt
  get = fmap (AccessToken . decodeUtf8) Data.Serialize.get

instance FromJSON AccessToken

instance ToJSON AccessToken

-- | An OAuth2 refresh token.
--
--   <https://tools.ietf.org/html/rfc6749#section-1.5>
newtype RefreshToken =
  RefreshToken Text
  deriving (Eq, Show, Generic)

instance FromJSON RefreshToken

instance ToJSON RefreshToken

-- | The type of an 'AccessToken'.
--
--   <https://tools.ietf.org/html/rfc6749#section-4.1.4>
data TokenType =
  Bearer
  deriving (Eq, Show)

instance FromJSON TokenType where
  parseJSON =
    withText "TokenType" $ \case
      "bearer" -> pure Bearer
      _ -> fail "Unsupported token type"

instance ToJSON TokenType where
  toJSON Bearer = "bearer"

-- | The expiration time of an 'AccessToken'.
newtype ExpirationTime =
  ExpirationTime Int
  deriving (Eq, Show, Generic)

instance FromJSON ExpirationTime

instance ToJSON ExpirationTime

-- | The authorization grant type.
--
--   <https://tools.ietf.org/html/rfc6749#section-1.3>
data GrantType =
  AuthorizationCodeGrantType

instance ToHttpApiData GrantType where
  toUrlPiece AuthorizationCodeGrantType = "authorization_code"

instance FromHttpApiData GrantType where
  parseUrlPiece "authorization_code" = Right AuthorizationCodeGrantType
  parseUrlPiece _ = Left "Unknown authorization code"

-- | The state values used to protect against cross-site request forgery.
--
--   <https://tools.ietf.org/html/rfc6749#section-4.1.1>
newtype CSRFState =
  CSRFState Text
  deriving (Show, Eq, ToHttpApiData, FromHttpApiData)

instance Serialize CSRFState where
  put (CSRFState txt) = Data.Serialize.put $ encodeUtf8 txt
  get = fmap (CSRFState . decodeUtf8) Data.Serialize.get

newtype Scope =
  Scope Text
  deriving (Show, Eq, IsString, ToHttpApiData, FromHttpApiData)
