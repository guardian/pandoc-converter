{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Provides a pandoc reader which can convert from the json representation
-- output by capi.
module Reader where

import Control.Monad.Except (throwError)
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Text.Pandoc
import Text.Pandoc.Sources (ToSources (toSources), sourcesToText)
import Text.Pandoc.UTF8 qualified as UTF8

import Capi qualified
import qualified Data.Map.Strict as Map

responseToPandoc :: Capi.Response -> IO Pandoc
responseToPandoc = \case
  Capi.Item Capi.ItemResponse{status, userTier, total, content} -> let
    meta = Meta (Map.fromList [ ("status", MetaString status)
                              , ("userTier", MetaString userTier)
                              , ("total", MetaString (T.pack (show total)))
                              ])
    in contentToPandoc content meta
  x -> return
    (Pandoc mempty
     [ Header 1 nullAttr [Str "Unsupported CAPI response type"]
     , Para [Str ("Got response of unsupported type: " <> T.pack (show x))]
     ])

contentToPandoc :: Capi.Content -> Meta -> IO Pandoc
contentToPandoc Capi.Content{fields = Capi.ContentFields{..}} meta = do
  standfirstBlocks <- Capi.parseHtml standfirst
  bylineBlocks <- Capi.parseHtml bylineHtml
  bodyBlocks <- Capi.parseHtml body
  mainBlocks <- Capi.parseHtml main
  return (Pandoc meta
    (concat
      [ [Header 1 nullAttr [Str headline]]
      , standfirstBlocks
      , mainBlocks
      , bylineBlocks
      , bodyBlocks
      ]))

readPandocFromJSON :: (PandocMonad m, ToSources a)
         => ReaderOptions
         -> a
         -> m Capi.Content
readPandocFromJSON _ s =
  case eitherDecode' . BL.fromStrict . UTF8.fromText
                     . sourcesToText . toSources $ s of
       Right doc -> return doc
       Left e    -> throwError $ PandocParseError ("JSON parse error (reading capi doc): "
                                                   <> T.pack e)
