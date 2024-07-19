module Composer where

data Block = Block
  { elements :: [Element]
  }

data Element = Element
  { elementType :: ElementType
  }

data ElementType
  = Text
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
