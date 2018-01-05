{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Description : API of an OAuth provider.

The API description of an OAuth2 provider supporting the 'authorization grant'
flow.
The primary purpose of this API description is to use it in combination with
"Servant.Client" to make requests to an OAuth2 provider.

This module also provides a combinator for marking APIs as OAuth2 protected.

-}
module OAuth2
  ( module OAuth2.Types
  , module OAuth2.Protect
  ) where

import "this" OAuth2.Protect
import "this" OAuth2.Types
