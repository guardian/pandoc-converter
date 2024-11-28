{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Control.Category ((>>>))
import Control.Monad.Trans.State.Strict
import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant hiding (Header)
import qualified Servant
import Text.Pandoc hiding (trace)

import Composer qualified
import Data.Functor ((<&>))
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = run 9482 app

app :: Application
app = serve converterAPI server

converterAPI :: Proxy ConverterAPI
converterAPI = Proxy

server :: Server ConverterAPI
server = return "working, hopefully"
  :<|> return "working, hopefully"
  :<|> exampleConversionHandler

type ConverterAPI = Get '[PlainText] Text
  :<|> "healthcheck" :> Get '[PlainText] Text
  :<|> "convert"
    :> ReqBody '[PlainText] Text
    :> Post '[PlainText] (Headers '[Servant.Header "Access-Control-Allow-Origin" Text] Text)
    -- assume markdown input and composer output for now

exampleConversionHandler :: Text -> Handler (Headers '[Servant.Header "Access-Control-Allow-Origin" Text] Text)
exampleConversionHandler input = do
  result <- liftIO (exampleConversion input)
  return (addHeader "*" result)

exampleConversion :: Text -> IO Text
exampleConversion input =
  mdToComposer input
    <&> (id
      >>> encode
      >>> toStrict
      >>> decodeUtf8)
  where
    -- toContentEntityRaw :: Composer.Block -> ContentEntityRaw
    -- toContentEntityRaw =

mdToComposer :: Text -> IO Composer.Block
mdToComposer txt = runIOorExplode $
    readMarkdown readerOptions txt
    >>= writeComposer def
  where
    readerOptions :: ReaderOptions
    readerOptions = def {readerExtensions = pandocExtensions}

writeComposer :: PandocMonad m => WriterOptions -> Pandoc -> m Composer.Block
writeComposer writerOptions document =
  evalStateT (pandocToComposer document) (WriterState writerOptions)

newtype WriterState = WriterState
  { options :: WriterOptions }

pandocToComposer :: PandocMonad m => Pandoc -> StateT WriterState m Composer.Block
pandocToComposer (Pandoc _meta blocks) = blocksToComposer blocks

blocksToComposer :: PandocMonad m => [Block] -> StateT WriterState m Composer.Block
blocksToComposer = traverse blockToComposer >>> fmap (mconcat >>> Composer.Block)

blockToComposer :: PandocMonad m => Block -> StateT WriterState m Composer.Elements
blockToComposer = \case
  Para inlines -> do
    wrapComposerText
      (\t -> "<p>" <> t <> "</p>")
      (fmap mconcat (traverse inlineToComposer inlines))
  Plain inlines -> wrapComposerText
    (\t -> "<p>" <> t <> "</p>")
    (fmap mconcat (traverse inlineToComposer inlines))
  LineBlock _ -> return mempty
  CodeBlock _ _ -> return mempty
  RawBlock _ _ -> return mempty
  BlockQuote _ -> return mempty
  BulletList blocks -> let
    listItem :: PandocMonad m => [Block] -> StateT WriterState m Composer.Elements
    listItem bs = wrapComposerText
      (\t -> "<li>" <> t <> "</li>")
      (fmap mconcat (traverse blockToComposer bs))
    list :: PandocMonad m => [StateT WriterState m Composer.Elements] -> StateT WriterState m Composer.Elements
    list items = wrapComposerText
      (\t -> "<ul>" <> t <> "</ul>")
      (fmap mconcat (sequence items))
    in list (fmap listItem blocks)
  OrderedList _ _ -> return mempty
  DefinitionList _ -> return mempty
  Header _ _ _ -> return mempty
  HorizontalRule -> return mempty
  Table _ _ _ _ _ _ -> return mempty
  Figure _ _ _ -> return mempty
  Div _ _ -> return mempty

wrapComposerText ::
  PandocMonad m =>
  (Text -> Text) ->
  StateT WriterState m Composer.Elements ->
  StateT WriterState m Composer.Elements
wrapComposerText f m = do
  Composer.Elements es <- m
  return (Composer.Elements (case es of
                               [Composer.Text t] -> [Composer.Text (f t)]
                               x -> x))

inlineToComposer :: PandocMonad m => Inline -> StateT WriterState m Composer.Elements
inlineToComposer = \case
  Str t -> return (Composer.Elements [Composer.Text t])
  Emph inlines ->
    wrapComposerText
      (\t -> "<em>" <> t <> "</em>")
      (fmap mconcat (traverse inlineToComposer inlines))
  Underline _ -> return mempty
  Strong inlines ->
    wrapComposerText
      (\t -> "<strong>" <> t <> "</strong>")
      (fmap mconcat (traverse inlineToComposer inlines))
  Strikeout _ -> return mempty
  Superscript inlines -> wrapComposerText
    (\t -> "<sup>" <> t <> "</sup>")
    (fmap mconcat (traverse inlineToComposer inlines))
  Subscript inlines -> wrapComposerText
    (\t -> "<sub>" <> t <> "</sub>")
    (fmap mconcat (traverse inlineToComposer inlines))
  SmallCaps _ -> return mempty
  Quoted _ _ -> return mempty
  Cite _ _ -> return mempty
  Code _ _ -> return mempty
  Space -> return (Composer.Elements [Composer.Text " "])
  SoftBreak -> return mempty
  LineBreak -> return mempty
  Math _ _ -> return mempty
  RawInline _ _ -> return mempty
  Link _ _ _ -> return mempty
  Image _ _ _ -> return mempty
  Note _ -> return mempty
  Span _ _ -> return mempty
