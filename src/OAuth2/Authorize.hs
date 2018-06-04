{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Description : Authorize endpoint of an OAuth2 provider.
-}
module OAuth2.Authorize where

import "base" Control.Monad.Fail (fail)
import "aeson" Data.Aeson
       (FromJSON(parseJSON), ToJSON(toJSON), withText)
import qualified "text" Data.Text as T
import "base" GHC.Exts (IsList(Item, fromList, toList))
import "this" OAuth2.Types
       (CSRFState, ClientId, RedirectURI, Scope)
import "protolude" Protolude
import "servant" Servant.API
       ((:>), FromHttpApiData(parseUrlPiece), Header, Headers, JSON,
        NoContent, QueryParam, StdMethod(GET), ToHttpApiData(toUrlPiece),
        Verb)

-- | Specification of the /autorize endpoint
--
--   <https://tools.ietf.org/html/rfc6749#section-4.1.1>
type API
   = QueryParam "client_id" ClientId
     :> QueryParam "response_type" ResponseType
     :> QueryParam "redirect_uri" RedirectURI
     :> QueryParam "state" CSRFState
     :> QueryParam "scopes" Scopes
     :> GetFound '[ JSON] (Headers '[ Header "Location" RedirectURI] NoContent)

-- | A Servant 'Verb' indicating this endpoint is expected to redirect the user.
type GetFound = Verb 'GET 302

-- | The desired grant type.
--
--   <https://tools.ietf.org/html/rfc6749#section-3.1.1>
data ResponseType =
  Code
  deriving (Show)

-- | The Client type generated
instance ToHttpApiData ResponseType where
  toUrlPiece Code = "code"

instance FromHttpApiData ResponseType where
  parseUrlPiece "code" = Right Code
  parseUrlPiece _ = Left "Unknown response type"

instance FromJSON ResponseType where
  parseJSON =
    withText "ResponseType" $ \case
      "code" -> pure Code
      _ -> fail "Unknown response type"

instance ToJSON ResponseType where
  toJSON Code = "code"

newtype Scopes =
  Scopes [Scope]
  deriving (Show, Eq)

instance IsList Scopes where
  type Item Scopes = Scope
  fromList = Scopes
  toList (Scopes scopes) = scopes

instance ToHttpApiData Scopes where
  toUrlPiece (Scopes scopes) = mconcat . intersperse " " $ toUrlPiece <$> scopes

instance FromHttpApiData Scopes where
  parseUrlPiece = fmap Scopes . traverse parseUrlPiece . T.words
