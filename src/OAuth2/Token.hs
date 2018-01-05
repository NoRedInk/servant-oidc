{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Description : Token endpoint of an OAuth2 provider.
-}
module OAuth2.Token where

import "aeson" Data.Aeson
       (FromJSON(parseJSON), ToJSON(toJSON), (.:), (.:?), (.=), object,
        withObject)
import qualified "jose-jwt" Jose.Jwt
import "this" OAuth2.Types
       (AccessToken, AuthorizationCode, ClientId, ClientSecret,
        ExpirationTime, GrantType, RedirectURI, RefreshToken, TokenType)
import "protolude" Protolude
import "servant" Servant.API
       ((:>), FormUrlEncoded, JSON, Post, ReqBody,
        ToHttpApiData(toQueryParam))
import "http-api-data" Web.FormUrlEncoded
       (FromForm(fromForm), ToForm(toForm), parseUnique)

-- | Specification of the /token endpoint
--
--   <https://tools.ietf.org/html/rfc6749#section-4.1.3>
type API
   = ReqBody '[ FormUrlEncoded] Request
     :> Post '[ JSON] Response

data Request = Request
  { requestGrantType :: GrantType
  , requestCode :: AuthorizationCode
  , requestRedirectURI :: RedirectURI
  , clientId :: ClientId
  , clientSecret :: ClientSecret
  }

instance ToForm Request where
  toForm Request { requestGrantType
                 , requestCode
                 , requestRedirectURI
                 , clientId
                 , clientSecret
                 } =
    [ ("grant_type", toQueryParam requestGrantType)
    , ("code", toQueryParam requestCode)
    , ("redirect_uri", toQueryParam requestRedirectURI)
    , ("client_id", toQueryParam clientId)
    , ("client_secret", toQueryParam clientSecret)
    ]

instance FromForm Request where
  fromForm f =
    Request <$> parseUnique "grant_type" f <*> parseUnique "code" f <*>
    parseUnique "redirect_uri" f <*>
    parseUnique "client_id" f <*>
    parseUnique "client_secret" f

-- | The response to a request for a new token.
--
--   <https://tools.ietf.org/html/rfc6749#section-4.1.4>
data Response = Response
  -- TODO: the access token, its type, and its expiration date should really be together in a single type.
  -- The optionally returned refresh token is something else.
  { responseAccessToken :: AccessToken
  , responseTokenType :: TokenType
  , responseExpiresIn :: Maybe ExpirationTime
  , responseRefreshToken :: Maybe RefreshToken
  , responseIdToken :: Jose.Jwt.Jwt
  } deriving (Eq, Show)

instance FromJSON Response where
  parseJSON =
    withObject "Response" $ \v ->
      Response <$> v .: "access_token" <*> v .: "token_type" <*>
      v .:? "expires_in" <*>
      v .:? "refresh_token" <*>
      v .: "id_token"

instance ToJSON Response where
  toJSON r =
    object
      [ "access_token" .= responseAccessToken r
      , "token_type" .= responseTokenType r
      , "expires_in" .= responseExpiresIn r
      , "refresh_token" .= responseRefreshToken r
      , "id_token" .= responseIdToken r
      ]
