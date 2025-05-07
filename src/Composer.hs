{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
-- | Redefinition of some parts of the composer model defined in
-- com.gu.flexiblecontent.model (in flexible-content-common)
module Composer where

import GHC.Generics (Generic)
import Data.Aeson
import Data.Text qualified as T
import Data.Time.Clock.System (SystemTime)
import Data.Map (Map)
import Data.Text (Text)
import Servant (ToHttpApiData (..))
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
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

newtype ContentId = ContentId Text
  deriving (Show, Generic)

instance ToJSON ContentId

instance ToHttpApiData ContentId where
  toUrlPiece (ContentId id) = id

data Block = Block
  { elements :: Elements,
    id :: BlockId,
    lastModified :: SystemTime,
    dateCreated :: SystemTime,
    publishedDate :: Maybe SystemTime,
    firstPublishedDate :: Maybe SystemTime,
    createdBy :: Maybe UserEntity,
    lastModifiedBy :: Maybe UserEntity,
    contributors :: [TagEntity],
    tags :: [TagEntity],
    published :: Bool,
    attributes :: Map Text Text,
    revisionId :: Maybe Int
  }
  deriving (Show, Generic)

instance ToJSON Block
instance FromJSON Block

newtype BlockId = BlockId Text
  deriving (Show, Generic)

instance ToJSON BlockId
instance FromJSON BlockId

instance ToHttpApiData BlockId where
  toUrlPiece (BlockId id) = id

-- While the Block type (known as BlockEntity) is accepted by live block
-- endpoint, BlockFragment is what's accepted by the draft block endpoint.
-- Annoyingly they're separate and slightly different?
data BlockFragment = BlockFragment
  { lastModifiedBy :: UserEntity,
    lastModified :: Maybe SystemTime,
    elements :: Maybe [ElementFragment],
    attributes :: Maybe (Map Text Text),
    contributors :: [TagEntity],
    tags :: [TagEntity],
    revisionId :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON BlockFragment

data UserEntity = UserEntity
  {email  :: Text,
    firstName :: Text,
    lastName :: Text
  } deriving (Show, Generic)

instance ToJSON UserEntity
instance FromJSON UserEntity

data ElementFragment = ElementFragment
  { elementType :: ElementType,
    fields :: Maybe ElementFields,
    assets :: Maybe [AssetFragment]
  }
  deriving (Show, Generic)

instance ToJSON ElementFragment

newtype ElementFields = ElementFields (Map Text Text)
  deriving (Show, Generic)

instance ToJSON ElementFields

-- not using yet
data TagEntity = TagEntity
  -- { id :: Int64,
  --   tagType :: String, -- called type
  --   subType :: Maybe String,
  --   internalName :: String,
  --   externalName :: String,
  --   slug :: Maybe String,
  --   section :: SectionEntity,
  --   path :: Maybe String,
  --   adBlockingLevel :: Maybe BlockingLevel,
  --   contributionBlockingLevel :: Maybe BlockingLevel
  -- }
  deriving (Show, Generic)
instance ToJSON TagEntity
instance FromJSON TagEntity
data AssetFragment
  deriving (Show, Generic)
instance ToJSON AssetFragment
data SectionEntity
  deriving (Show, Generic)
instance ToJSON SectionEntity
data BlockingLevel
  deriving (Show, Generic)
instance ToJSON BlockingLevel

newtype Elements = Elements [Element]
  deriving (Show, Generic)

instance ToJSON Elements where
  toJSON (Elements es) = toJSON es

instance FromJSON Elements

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
  deriving (Show)

instance ToJSON Element where
  toJSON e = object
    ([ "elementType" .= elementToElementType e
     ]
     <> (case e of Text t -> ["fields" .= object ["text" .= t]]; _nonText -> []))

elementToElementFragment :: Element -> ElementFragment
elementToElementFragment e = let
  elementType = elementToElementType e
  fields = case e of
    Text t -> Just (ElementFields (Map.singleton "text" t))
    _ -> Nothing
  in ElementFragment{assets = Nothing, ..}

elementToElementType :: Element -> ElementType
elementToElementType = \case
  Text _ -> TextType
  Image -> ImageType
  Embed -> EmbedType
  Form -> FormType
  PullQuote -> PullQuoteType
  Interactive -> InteractiveType
  Comment -> CommentType
  RichLink -> RichLinkType
  Table -> TableType
  Video -> VideoType
  Tweet -> TweetType
  Witness -> WitnessType
  Code -> CodeType
  Audio -> AudioType
  Map -> MapType
  Document -> DocumentType
  Membership -> MembershipType
  ContentAtom -> ContentAtomType
  Instagram -> InstagramType
  Vine -> VineType
  Callout -> CalloutType
  Cartoon -> CartoonType
  Recipe -> RecipeType
  List -> ListType
  Timeline -> TimelineType

data ElementType
  = TextType
  | ImageType
  | EmbedType
  | FormType
  | PullQuoteType
  | InteractiveType
  | CommentType
  | RichLinkType
  | TableType
  | VideoType
  | TweetType
  | WitnessType
  | CodeType
  | AudioType
  | MapType
  | DocumentType
  | MembershipType
  | ContentAtomType
  | InstagramType
  | VineType
  | CalloutType
  | CartoonType
  | RecipeType
  | ListType
  | TimelineType
  deriving (Show, Eq, Generic)

typenames :: [(ElementType, Text)]
typenames =
  [ (TextType, "text"),
    (ImageType, "image"),
    (EmbedType, "embed"),
    (FormType, "form"),
    (PullQuoteType, "pullQuote"),
    (InteractiveType, "interactive"),
    (CommentType, "comment"),
    (RichLinkType, "richLink"),
    (TableType, "table"),
    (VideoType, "video"),
    (TweetType, "tweet"),
    (WitnessType, "witness"),
    (CodeType, "code"),
    (AudioType, "audio"),
    (MapType, "map"),
    (DocumentType, "document"),
    (MembershipType, "membership"),
    (ContentAtomType, "contentAtom"),
    (InstagramType, "instagram"),
    (VineType, "vine"),
    (CalloutType, "callout"),
    (CartoonType, "cartoon"),
    (RecipeType, "recipe"),
    (ListType, "list"),
    (TimelineType, "timeline")
  ]

elementTypes :: [(Text, ElementType)]
elementTypes = fmap (\(a, b) -> (b, a)) typenames

instance ToJSON ElementType where
  toJSON elementType = toJSON typename
    where
      typename :: Text
      typename = fromMaybe "unknown-element" (lookup elementType typenames)

instance FromJSON ElementType where
  parseJSON = withText "ElementType" \t -> maybe (fail "unknown element") return (lookup t elementTypes)

instance FromJSON Element where
  parseJSON = withObject "Element" \o -> do
    elementType <- o .: "elementType"
    case elementType of
      TextType -> do
        fields <- o .: "fields"
        t <- fields .: "text"
        return (Text t)
      ImageType -> return Image
      EmbedType -> return Embed
      FormType -> return Form
      PullQuoteType -> return PullQuote
      InteractiveType -> return Interactive
      CommentType -> return Comment
      RichLinkType -> return RichLink
      TableType -> return Table
      VideoType -> return Video
      TweetType -> return Tweet
      WitnessType -> return Witness
      CodeType -> return Code
      AudioType -> return Audio
      MapType -> return Map
      DocumentType -> return Document
      MembershipType -> return Membership
      ContentAtomType -> return ContentAtom
      InstagramType -> return Instagram
      VineType -> return Vine
      CalloutType -> return Callout
      CartoonType -> return Cartoon
      RecipeType -> return Recipe
      ListType -> return List
      TimelineType -> return Timeline
