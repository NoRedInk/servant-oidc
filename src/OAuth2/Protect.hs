{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Description : Combinator for adding OAuth2 protection to a Servant API.
-}
module OAuth2.Protect where

import qualified "text" Data.Text as T
import "this" OAuth2.Types
       (AccessToken(AccessToken), TokenType(Bearer), TokenType)
import "protolude" Protolude
import "servant" Servant.API
       (FromHttpApiData(parseUrlPiece), Header, ToHttpApiData(toUrlPiece))

-- | A combinator for marking an endpoint as requiring OAuth2 authorization.
--
--   TODO: Because this combinator uses `Header` directly, clients using it will
--   have to pass a `Maybe Authorization`. We should see if we can make a custom
--   combinator that just takes an `Authorization`.
type Protect = Header "Authorization" Authorization

-- | The authorization data passed in requests to OAuth2 protected endpoints.
data Authorization = Authorization
  { authorizationAccessToken :: AccessToken
  , authorizationTokenType :: TokenType
  }

instance ToHttpApiData Authorization where
  toUrlPiece authorization =
    case authorization of
      Authorization { authorizationTokenType = Bearer
                    , authorizationAccessToken = AccessToken accessToken
                    } -> "Bearer " <> accessToken

instance FromHttpApiData Authorization where
  parseUrlPiece authorization =
    case T.splitOn " " authorization of
      ["Bearer", token] ->
        Right
          Authorization
          { authorizationAccessToken = AccessToken $ toS token
          , authorizationTokenType = Bearer
          }
      _ -> Left "Invalid authorization string"
