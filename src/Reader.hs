{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}

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
import Text.Pandoc.Walk (walk)

import Capi qualified
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid (First(..))
import Data.Functor ((<&>))

responseToPandoc :: Capi.Response -> IO Pandoc
responseToPandoc = \case
  Capi.Item Capi.ItemResponse{status, userTier, total, content, results, tag, section} -> let
    meta = Meta (Map.fromList [ ("status", MetaString status)
                              , ("userTier", MetaString userTier)
                              , ("total", MetaString (T.pack (show total)))
                              ])
    in case content of
      Just c -> contentToPandoc c meta
      Nothing -> do
        let preamble = getFirst (foldMap First
              [ fmap sectionPreamble section
              , fmap tagPreamble tag
              ] )
        resultBlocks <- maybe (return []) (traverse (contentToBlocks 2)) results
        return (Pandoc mempty (fromMaybe [] preamble <> concat resultBlocks))
  x -> return
    (Pandoc mempty
     [ Header 1 nullAttr [Str "Unsupported CAPI response type"]
     , Para [Str ("Got response of unsupported type: " <> T.pack (show x))]
     ])

sectionPreamble :: Capi.Section -> [Block]
sectionPreamble Capi.Section{..} =
  [ Header 1 nullAttr [Str ("Section: " <> webTitle)]
  , Para [Str "Here are the things in this section:"]
  ]

tagPreamble :: Capi.Tag -> [Block]
tagPreamble t@Capi.Tag {..} =
  case _type of
      Capi.Contributor -> catMaybes
        [ Just (Header 1 nullAttr [Str ("Contributor: " <> webTitle)])
        , fmap (\(Capi.HtmlAsText b) -> Para [Str b]) bio
        , Just (Para [Str ("Here is recent content by " <> webTitle)])
        ]
      _other ->
        [ Header 1 nullAttr [Str ("Tag: "  <> webTitle)],
        Para [Str ("Tag details: " <> T.pack (show t))]
        ]

contentToPandoc :: Capi.Content -> Meta -> IO Pandoc
contentToPandoc content meta = do
  blocks <- contentToBlocks 1 content
  let updateMeta = case content.fields of
        Just Capi.ContentFields{headline = Just h} ->
          Map.insert "title" (MetaString h)
        _noHeadline -> id
  return (Pandoc (Meta (updateMeta (unMeta meta))) blocks)

resultsToPandoc :: Maybe [Capi.Content] -> Meta -> IO Pandoc
resultsToPandoc (Just results) meta = do
  resultBlocks <- traverse (contentToBlocks 2) results
  return
    (Pandoc meta
       (Header 1 nullAttr [Str "Results"] : concat resultBlocks))
resultsToPandoc Nothing meta = return (Pandoc meta [Para [Str "Nothing at all here: is this some other capi response type?"]])

contentToBlocks :: Int -> Capi.Content -> IO [Block]
contentToBlocks baseHeaderLevel Capi.Content {fields = Just Capi.ContentFields {..}, tags} = do
  let demoteHeadersBy n = \case
        Header m attrs contents -> Header (m + n) attrs contents
        x -> x
      demoteH1s = \case
        -- hides iframe-produced extra h1s in articles
        -- can remove for a nicer solution when no longer parsing the html with
        -- pandoc
        Header 1 attrs contents -> Header 2 attrs contents
        x -> x
  standfirstBlocks <- maybe (pure []) Capi.parseHtml standfirst
  bylineBlocks <- maybe (pure []) Capi.parseHtml bylineHtml
  bodyBlocks <- maybe (pure []) Capi.parseHtml body
  mainBlocks <- maybe (pure []) Capi.parseHtml main
  let tagBlocks =
        case tags of
          Nothing -> []
          Just ts ->
            [ Para [Str "Article tags:"]
            , BulletList (ts <&> \t -> [Para [Link nullAttr mempty ("capi-org:" <> t.id, "")]])
            ]
  return
    ( walk
        (demoteHeadersBy (baseHeaderLevel - 1))
        ( Header 1 nullAttr [Str (fromMaybe "" headline)]
            : ( walk
                  demoteH1s
                  ( concat
                      [ standfirstBlocks,
                        mainBlocks,
                        bylineBlocks,
                        bodyBlocks,
                        tagBlocks
                      ]
                  )
              )
        )
    )
contentToBlocks headerLevel c =
  return
    [ Header headerLevel nullAttr [Str "Unknown content"],
      Para [Str "No fields found, don’t know what this is!"],
      Para [Str (T.pack (show c))]
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
