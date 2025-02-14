{-# LANGUAGE ImportQualifiedPost #-}
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

contentToPandoc :: Capi.Content -> IO Pandoc
contentToPandoc Capi.Content{fields = Capi.ContentFields{..}} = do
  standfirstBlocks <- Capi.parseHtml standfirst
  bylineBlocks <- Capi.parseHtml bylineHtml
  bodyBlocks <- Capi.parseHtml body
  return (Pandoc mempty
    (concat [ [Header 1 nullAttr [Str headline]]
    , standfirstBlocks
    , bylineBlocks
    , bodyBlocks
    ]
    ))

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
