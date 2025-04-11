{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Redefinition of some parts of the content-api-models model
module Capi where

import Data.Aeson
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Text.Pandoc (Block (..), runIOorExplode, def, Pandoc (..))
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
  , content :: Maybe Content
  , results :: Maybe [Content]
  , section :: Maybe Section
  , tag :: Maybe Tag
  } deriving (Show, Generic)

instance FromJSON ItemResponse

data SearchResponse = SearchResponse
  deriving (Show, Generic)

data SectionsResponse = SectionsResponse
  deriving (Show, Generic)

data Content = Content
 { fields :: Maybe ContentFields
 , tags :: Maybe [Tag]
 }
 deriving (Show, Generic)

instance FromJSON Content

data ContentFields = ContentFields
  { headline :: Maybe Text
  , standfirst :: Maybe HtmlAsText
  , bylineHtml :: Maybe HtmlAsText
  , body :: Maybe HtmlAsText
  , bodyText :: Maybe Text
  , main :: Maybe HtmlAsText
  }
  deriving (Show, Generic)

instance FromJSON ContentFields

newtype HtmlAsText = HtmlAsText Text
  deriving (Show, FromJSON)

parseHtml :: HtmlAsText -> IO [Block]
parseHtml (HtmlAsText t) = do
  Pandoc _meta blocks <- runIOorExplode (readHtml def t)
  return blocks

data Section = Section
  { id :: Text
  , webTitle :: Text
  , webUrl :: Text
  , apiUrl :: Text
  }
  deriving (Show, Generic)

instance FromJSON Section

data Tag = Tag
  { id :: Text
  , _type :: TagType
  , webTitle :: Text
  , webUrl :: Text
  , apiUrl :: Text
  , description :: Maybe Text
  , bio :: Maybe HtmlAsText
  , firstName :: Maybe Text
  , lastName :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON Tag where
  parseJSON = withObject "Tag" \o -> do
    id <- o .: "id"
    _type <- o .: "type"
    webTitle <- o .: "webTitle"
    webUrl <- o .: "webUrl"
    apiUrl <- o .: "apiUrl"
    description <- o .:? "description"
    bio <- o .:? "bio"
    firstName <- o .:? "firstName"
    lastName <- o .:? "lastName"
    return Tag{..}

data TagType
  = Contributor
  | Keyword
  | Series
  | NewspaperBookSection
  | NewspaperBook
  | Blog
  | Tone
  | Type
  | Publication
  | Tracking
  | PaidContent
  | Campaign
  deriving (Show, Generic)

instance FromJSON TagType where
  parseJSON = withText "TagType" \t ->
    maybe
      (fail ("Unrecognised TagType: " <> Text.unpack t))
      return
      (lookup t
        [ ("contributor", Contributor)
        , ("keyword", Keyword)
        , ("series", Series)
        , ("newspaper-book-section", NewspaperBookSection)
        , ("newspaper-book", NewspaperBook)
        , ("blog", Blog)
        , ("tone", Tone)
        , ("type", Type)
        , ("publication", Publication)
        , ("tracking", Tracking)
        , ("paid-content", PaidContent)
        , ("campaign", Campaign)
        ])
