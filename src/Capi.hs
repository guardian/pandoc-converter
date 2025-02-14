-- | Redefinition of some parts of the content-api-models model
module Capi where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Coerce (coerce)
import Text.Pandoc (Block (..), Inline (..), runIOorExplode, def, Pandoc (..))
import Text.Pandoc.Readers (readHtml)

data Content = Content
 { fields :: ContentFields
 }
 deriving (Generic)

instance FromJSON Content

data ContentFields = ContentFields
  { headline :: Text
  , standfirst :: HtmlAsText
  , bylineHtml :: HtmlAsText
  , body :: HtmlAsText
  , bodyText :: Text
  }
  deriving (Generic)

instance FromJSON ContentFields

newtype HtmlAsText = HtmlAsText Text
  deriving (FromJSON)

parseHtml :: HtmlAsText -> IO [Block]
parseHtml (HtmlAsText t) = do
  Pandoc _meta blocks <- runIOorExplode (readHtml def t)
  return blocks
