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

readCapi ::
  (PandocMonad m, ToSources a) =>
  ReaderOptions ->
  a ->
  m Pandoc
readCapi options input = fmap contentToPandoc (readPandocFromJSON options input)

contentToPandoc :: Capi.Content -> Pandoc
contentToPandoc Capi.Content{fields = Capi.ContentFields{..}} =
  Pandoc mempty
    [ Header 1 nullAttr [Str headline]
    , Capi.parseHtml standfirst
    , Capi.parseHtml bylineHtml
    , Capi.parseHtml body
    ]

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
