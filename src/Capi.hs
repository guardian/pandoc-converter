-- | Redefinition of some parts of the content-api-models model
module Capi where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Coerce (coerce)
import Text.Pandoc (Block (..), Inline (..), runIOorExplode, def, Pandoc (..))
import Text.Pandoc.Readers (readHtml)

data EndpointWrapper = EndpointWrapper
  { response :: Response
  }
  deriving (Show, Generic)

instance FromJSON EndpointWrapper

data Response
  = Item ItemResponse
  | Search SearchResponse
  | Sections SectionsResponse
  deriving (Show, Generic)

instance FromJSON Response where
  parseJSON = fmap Item . parseJSON

data ItemResponse = ItemResponse
  { status :: Text
  , userTier :: Text
  , total :: Integer
  , content :: Content
  } deriving (Show, Generic)

instance FromJSON ItemResponse

data SearchResponse = SearchResponse
  deriving (Show, Generic)

data SectionsResponse = SectionsResponse
  deriving (Show, Generic)

data Content = Content
 { fields :: ContentFields
 }
 deriving (Show, Generic)

instance FromJSON Content

data ContentFields = ContentFields
  { headline :: Text
  , standfirst :: HtmlAsText
  , bylineHtml :: HtmlAsText
  , body :: HtmlAsText
  , bodyText :: Text
  , main :: HtmlAsText
  }
  deriving (Show, Generic)

instance FromJSON ContentFields

newtype HtmlAsText = HtmlAsText Text
  deriving (Show, FromJSON)

parseHtml :: HtmlAsText -> IO [Block]
parseHtml (HtmlAsText t) = do
  Pandoc _meta blocks <- runIOorExplode (readHtml def t)
  return blocks
