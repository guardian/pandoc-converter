{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Pandoc
import Data.Text (Text)
import qualified Data.Text.IO as T
import Control.Monad.Trans.State.Strict
import Control.Category ((>>>))

import Composer qualified

main :: IO ()
main = do
  T.getContents >>= mdToComposer >>= T.putStrLn

mdToComposer :: Text -> IO Text
mdToComposer txt = runIOorExplode $
  readMarkdown def txt
  >>= writeComposer def

writeComposer :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeComposer writerOptions document =
  evalStateT (pandocToComposer document) (WriterState writerOptions)

newtype WriterState = WriterState
  { options :: WriterOptions }

pandocToComposer :: PandocMonad m => Pandoc -> StateT WriterState m Text
pandocToComposer (Pandoc _meta blocks) = blocksToComposer blocks

blocksToComposer :: PandocMonad m => [Block] -> StateT WriterState m Text
blocksToComposer = traverse blockToComposer >>> fmap mconcat

blockToComposer :: PandocMonad m => Block -> StateT WriterState m Text
blockToComposer = \case
  Para inlines -> fmap mconcat (traverse inlineToComposer inlines)
  Plain _ -> return ""
  LineBlock _ -> return ""
  CodeBlock _ _ -> return ""
  RawBlock _ _ -> return ""
  BlockQuote _ -> return ""
  BulletList _ -> return ""
  OrderedList _ _ -> return ""
  DefinitionList _ -> return ""
  Header _ _ _ -> return ""
  HorizontalRule -> return ""
  Table _ _ _ _ _ _ -> return ""
  Figure _ _ _ -> return ""
  Div _ _ -> return ""

inlineToComposer :: PandocMonad m => Inline -> StateT WriterState m Text
inlineToComposer = \case
  Str t -> return t
  Emph _ -> return ""
  Underline _ -> return ""
  Strong _ -> return ""
  Strikeout _ -> return ""
  Superscript _ -> return ""
  Subscript _ -> return ""
  SmallCaps _ -> return ""
  Quoted _ _ -> return ""
  Cite _ _ -> return ""
  Code _ _ -> return ""
  Space -> return ""
  SoftBreak -> return ""
  LineBreak -> return ""
  Math _ _ -> return ""
  RawInline _ _ -> return ""
  Link _ _ _ -> return ""
  Image _ _ _ -> return ""
  Note _ -> return ""
  Span _ _ -> return ""
