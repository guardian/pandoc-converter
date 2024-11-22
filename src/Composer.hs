{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Composer where

import GHC.Generics (Generic)
import Data.Aeson
import Data.Map.Strict (Map)
import Data.Text qualified as T
-- import Data.Time

-- data ContentEntityRaw = ContentEntityRaw
--   { id :: String,
--     contentType :: String,
--     originatingSystem :: Maybe String,
--     published :: Bool,
--     isGone :: Maybe Bool,
--     isHosted :: Maybe Bool,
--     scheduledLaunchDate :: Maybe (), -- DateTime,
--     requestedScheduledLaunch :: Maybe (), -- DateTime,
--     expiry :: Maybe (), -- ExpiryEntityRaw,
--     rights :: Maybe (), -- RightsEntity,
--     contentChangeDetails :: (), -- ChangeDetailsEntityRaw,
--     identifiers :: Map String String,
--     collaborators :: [()], -- [UserEntity],
--     toolSettings :: Map String String,
--     aliasPaths :: [()], -- [AliasPath],
--     preview :: Maybe (), -- ContentFacetEntityRaw,
--     live :: Maybe (), -- ContentFacetEntityRaw,
--     auxiliaryAtoms :: [()], -- [AuxiliaryAtomEntity],
--     channels :: Maybe () -- ChannelsEntity.ChannelsData
--   }

-- instance ToJSON ContentEntityRaw where
--   toJSON (ContentEntityRaw {..}) = object
--     [ "id" .= id
--     , "type" .= contentType
--     , "originatingSystem" .= originatingSystem
--     , "published" .= published
--     , "isGone" .= isGone
--     , "isHosted" .= isHosted
--     , "scheduledLaunchDate" .= scheduledLaunchDate
--     , "requestedScheduledLaunch" .= requestedScheduledLaunch
--     , "expiry" .= expiry
--     , "rights" .= rights
--     , "contentChangeDetails" .= contentChangeDetails
--     , "identifiers" .= identifiers
--     , "collaborators" .= collaborators
--     , "toolSettings" .= toolSettings
--     , "aliasPaths" .= aliasPaths
--     , "preview" .= preview
--     , "live" .= live
--     , "auxiliaryAtoms" .= auxiliaryAtoms
--     , "channels" .= channels
--     ]

-- data ContentFacetEntityRaw = ContentFacetEntityRaw
--   {  contentChangeDetails :: ChangeDetailsEntityRaw,
--     fields :: Map String String,
--     thumbnail :: Maybe (), -- ImageEntity,
--     mainBlock :: Maybe (), -- BlockEntity,
--     blocks :: [Block],
--     settings :: Map String String,
--     taxonomy :: Maybe (), -- TaxonomyEntityRaw,
--     aliasPaths :: [()] -- [AliasPath]
--   }

data Block = Block
  { elements :: Elements
    -- id :: String,
                       -- lastModified :: UTCTime,
                       -- dateCreated :: UTCTime,
                       -- publishedDate :: Maybe UTCTime ,
                       -- firstPublishedDate :: Maybe UTCTime ,
                       -- createdBy :: Maybe UserEntity,
                       -- lastModifiedBy :: Maybe UserEntity,
                       -- contributors :: Seq TagEntity,
                       -- tags :: Seq TagEntity,
                       -- published :: Boolean,
                       -- attributes :: Map String String,
                       -- revisionId :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON Block

newtype Elements = Elements [Element]
  deriving Show

instance ToJSON Elements where
  toJSON (Elements es) = toJSON es

instance Semigroup Composer.Elements where
  (Composer.Elements e1) <> (Composer.Elements e2) = case (e1, e2) of
    ([Composer.Text t1], Composer.Text t2 : rest)
      -> Composer.Elements (Composer.Text (t1 <> t2) : rest)
    _ -> Composer.Elements (e1 <> e2)

instance Monoid Composer.Elements where
  mempty = Composer.Elements []

data Element
  = Text T.Text
  | Image
  | Embed
  | Form
  | PullQuote
  | Interactive
  | Comment
  | RichLink
  | Table
  | Video
  | Tweet
  | Witness
  | Code
  | Audio
  | Map
  | Document
  | Membership
  | ContentAtom
  | Instagram
  | Vine
  | Callout
  | Cartoon
  | Recipe
  | List
  | Timeline
  deriving Show

instance ToJSON Element where
  toJSON e = object
    ([ "elementType" .= elementType e
     ]
     <> (case e of Text t -> ["fields" .= object ["text" .= t]]; _nonText -> []))
    where
      elementType :: Element -> T.Text
      elementType = \case
        Text _ -> "text"
        Image -> "image"
        Embed -> "embed"
        Form -> "form"
        PullQuote -> "pullQuote"
        Interactive -> "interactive"
        Comment -> "comment"
        RichLink -> "richLink"
        Table -> "table"
        Video -> "video"
        Tweet -> "tweet"
        Witness -> "witness"
        Code -> "code"
        Audio -> "audio"
        Map -> "map"
        Document -> "document"
        Membership -> "membership"
        ContentAtom -> "contentAtom"
        Instagram -> "instagram"
        Vine -> "vine"
        Callout -> "callout"
        Cartoon -> "cartoon"
        Recipe -> "recipe"
        List -> "list"
        Timeline -> "timeline"
