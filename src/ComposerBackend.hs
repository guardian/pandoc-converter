{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BlockArguments #-}
-- | Servant definition of some parts of the composer-backend API.
module ComposerBackend where

import Composer
import Servant hiding (addHeader)
import Servant.Client
import Servant.Client.Core (AuthenticatedRequest, AuthClientData, addHeader)
import qualified Data.Text as Text
import Servant.Client.Core.Request (Request)
import Network.HTTP.Types (hCookie)
import Data.Aeson

composerBackendAPI :: Proxy ComposerBackendAPI
composerBackendAPI = Proxy

type ComposerBackendAPI =
  AuthProtect "panda-cookie"
    :> "api"
    :> "content"
    :> Capture "content-id" ContentId
    :> "preview"
    :> "blocks"
    :> Capture "block-id" BlockId
    :> ReqBody '[JSON] BlockFragment
    :> Post '[JSON] WrappedBlock
  :<|> AuthProtect "panda-cookie"
    :> "api"
    :> "contentRaw"
    :> Capture "content-id" ContentId
    :> QueryParam "includePreview" Bool
    :> QueryParam "includeLive" Bool
    :> Get '[JSON] ContentEntityRaw

postBlock ::
  AuthenticatedRequest (AuthProtect "panda-cookie") ->
  ContentId ->
  BlockId ->
  BlockFragment ->
  ClientM WrappedBlock

getContent ::
  AuthenticatedRequest (AuthProtect "panda-cookie") ->
  ContentId ->
  Maybe Bool ->
  Maybe Bool ->
  ClientM ContentEntityRaw


postBlock :<|> getContent = client composerBackendAPI

type instance AuthClientData (AuthProtect "panda-cookie") = Text.Text

authenticate :: Text.Text -> Request -> Request
authenticate cookie = addHeader hCookie fullCookie
  where fullCookie = "gutoolsAuth-assym=" <> cookie

newtype WrappedBlock = WrappedBlock Block
  deriving (Show)

instance FromJSON WrappedBlock where
  parseJSON = withObject "WrappedBlock" \o -> do
    _data <- o .: "data"
    block <- _data .: "block"
    return (WrappedBlock block)
