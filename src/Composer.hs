{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Composer where

import Data.Text qualified as T
import GHC.Generics (Generic)
import Data.Aeson

data Block = Block
  { elements :: Elements
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
