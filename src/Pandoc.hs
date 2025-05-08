{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Pandoc readers and writers
module Pandoc where

import Text.Pandoc

import qualified Composer
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as Text
import Data.Aeson (FromJSON)

composerToPandoc :: Composer.ContentEntityRaw -> IO Pandoc
composerToPandoc Composer.ContentEntityRaw{preview, id} = do
  let (firstBlockId, blockRevisionId) = fromMaybe ("oops", Nothing) do
        Composer.ContentFacetEntityRaw{blocks} <- preview
        Composer.BlockEntity{revisionId, id = blockId} <- listToMaybe blocks
        return (blockId, revisionId)
  let title = fromMaybe "no-title-found" do
        Composer.ContentFacetEntityRaw{fields} <- preview
        Map.lookup "headline" fields

  let meta = Meta (Map.fromList [ ("title", MetaInlines [Str "hello?"])
                                ])
  let propertiesBlocks =
        fmap Plain [ [Str ":PROPERTIES:"]
              , [Str ":CONTENT_ID:", Space, Str id]
              , [ Str ":REVISION_ID:"
                , Space
                , Str (Text.pack (show (fromMaybe (-1) blockRevisionId)))
                ]
              , [Str ":MAIN_BLOCK_ID:", Space, Str firstBlockId]
              , [Str ":END:"]
              ]
  let titleBlocks = [RawBlock (Format "org") ("#+title: " <> title <> "\n\n")]
  blocks <- fromMaybe (return []) do
    Composer.ContentFacetEntityRaw{blocks} <- preview
    return (fmap concat (traverse blockEntityToBlocks blocks))
  return (Pandoc meta (propertiesBlocks <> titleBlocks <> blocks))

blockEntityToBlocks :: Composer.BlockEntity -> IO [Block]
blockEntityToBlocks Composer.BlockEntity{elements} =
  fmap concat (traverse elementToBlocks elements)

elementToBlocks :: Composer.Element -> IO [Block]
elementToBlocks = \case
  Composer.Text t -> parseHtml (HtmlAsText t)
  e -> return [Para [Str "Unsupported element:", Space, Str (Text.pack (show e))]]

newtype HtmlAsText = HtmlAsText Text.Text
  deriving (Show, FromJSON)

parseHtml :: HtmlAsText -> IO [Block]
parseHtml (HtmlAsText t) = do
  Pandoc _meta blocks <- runIOorExplode (readHtml def t)
  return blocks
