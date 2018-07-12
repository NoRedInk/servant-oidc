{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Description : keys endpoint of a OpenID Connect provider.
-}
module OpenIdConnect.Keys where

import "aeson" Data.Aeson (FromJSON, ToJSON)
import "base" GHC.Generics (Generic)
import "jose-jwt" Jose.Jwk (Jwk)
import "servant" Servant.API (Get, JSON)

type API = Get '[ JSON] Response

newtype Response = Response
  { keys :: [Jwk]
  } deriving (Generic)

instance FromJSON Response

instance ToJSON Response
