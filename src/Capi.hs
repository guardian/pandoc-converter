-- | Redefinition of some parts of the content-api-models model
module Capi where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Coerce (coerce)
import Text.Pandoc (Block (..), Inline (..))

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

parseHtml :: HtmlAsText -> Block
parseHtml h = Para [Str (coerce h)]
