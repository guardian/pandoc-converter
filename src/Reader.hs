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
import Pandoc qualified
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

responseToPandocs :: Capi.Response -> IO [Pandoc]
responseToPandocs = \case
  Capi.Item Capi.ItemResponse{status, userTier, total, content, results, tag, section} -> let
    meta = Meta (Map.fromList [ ("status", MetaString status)
                              , ("userTier", MetaString userTier)
                              , ("total", MetaString (T.pack (show total)))
                              ])
    in case content of
      Just c -> (: []) <$> contentToPandoc c meta
      Nothing -> do
        let preamble = getFirst (foldMap First
              [ fmap sectionPreamble section
              , fmap tagPreamble tag
              ] )
        resultBlocks <- maybe (return []) (traverse (contentToBlocks 1)) results
        return (Pandoc mempty (fromMaybe [] preamble)
                : fmap (Pandoc mempty) resultBlocks)
  x -> return
    [Pandoc mempty
     [ Header 1 nullAttr [Str "Unsupported CAPI response type"]
     , Para [Str ("Got response of unsupported type: " <> T.pack (show x))]
     ]]

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
        , fmap (\(Pandoc.HtmlAsText b) -> Para [Str b]) bio
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
contentToBlocks baseHeaderLevel Capi.Content
  { fields = Just Capi.ContentFields {..},
    tags,
    blocks,
    webPublicationDate
  } = do
  let demoteHeadersBy n = \case
        Header m attrs contents -> Header (m + n) attrs contents
        x -> x
      demoteH1s = \case
        -- hides iframe-produced extra h1s in articles
        -- can remove for a nicer solution when no longer parsing the html with
        -- pandoc
        Header 1 attrs contents -> Header 2 attrs contents
        x -> x
  standfirstBlocks <- maybe (pure []) Pandoc.parseHtml standfirst
  bylineBlocks <- maybe (pure []) Pandoc.parseHtml bylineHtml
  -- bodyBlocks <- maybe (pure []) Pandoc.parseHtml body
  bodyBlocks <- case blocks of
        Nothing -> return [ Para [Str "No blocks found"] ]
        (Just Capi.Blocks{body = Nothing}) -> return [ Para [Str "No body blocks found"] ]
        (Just Capi.Blocks{body = Just body}) ->
          fmap concat (traverse capiBlockToBlock body) -- TODO: handle multiple blocks better?

  mainBlocks <- maybe (pure []) Pandoc.parseHtml main
  let tagBlocks =
        case tags of
          Nothing -> []
          Just ts ->
            [ Para [Str "Article tags:"]
            , BulletList (ts <&> \t -> [Para [Link nullAttr [Str t.id] ("capi-org:" <> t.id, "Tag: " <> t.id)]])
            ]
  let publishTimeBlocks = case webPublicationDate of
        Nothing -> [Para [Str "(No publication dates)"]]
        Just t -> [Para [Str ("Published: " <> T.pack (show t)
                               <> maybe
                                 ""
                                 (\t -> " (last modified: " <> T.pack (show t) <> ")")
                                 lastModified
                             )]]

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
                        publishTimeBlocks,
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

capiBlockToBlock :: Capi.Block -> IO [Block]
capiBlockToBlock Capi.Block{elements} =
  fmap concat (traverse capiBlockElementToBlock elements)

capiBlockElementToBlock :: Capi.BlockElement -> IO [Block]
capiBlockElementToBlock = \case
  Capi.TextElement Capi.TextElementFields{html} ->
    maybe (return [ Para [Str "(Empty text element)"] ]) Pandoc.parseHtml html
  Capi.ImageElement Capi.ImageElementFields{caption, alt, mediaApiUri} ->
    return [Figure
            mempty
            (Caption Nothing (maybe [] (\c -> [ Para [Str c] ]) caption))
            [ Para [
               Image mempty [Str (fromMaybe (fromMaybe "" alt) caption)] (fromMaybe "" mediaApiUri, fromMaybe "" alt) ]]]
  Capi.VideoElement Capi.VideoElementFields{url, title, description} ->
    return [ Figure
             mempty
             (Caption Nothing (maybe [] (\d -> [ Para [ Str d ] ]) description))
             [ Para
               [ Link
                 mempty
                 (maybe [] (\t -> [Str t]) title)
                 (fromMaybe "" url, fromMaybe "" title)
               ]
             ]
           ]
  Capi.UnknownBlockElement elementType ->
    return [ Para [Str ("(Unknown block element: " <> elementType <> ")")]  ]

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
