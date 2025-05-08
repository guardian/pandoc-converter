{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Category ((>>>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict
import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO qualified as Text
import Network.Wai
import Network.Wai.Handler.Warp
import Servant hiding (Header)
import Servant qualified
import Text.Pandoc hiding (TextWriter, trace)
import Text.Pandoc qualified
import Text.Pandoc.Walk (walk)

import Capi qualified
import Composer qualified
import Reader qualified
import System.Environment (getArgs, getEnv)
import Data.Foldable (for_)
import qualified Data.Map as Map
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import qualified ComposerBackend
import Servant.Client.Core (mkAuthenticatedRequest)
import Data.Time.Clock.System (SystemTime(MkSystemTime))
import Composer (elementToElementFragment)
import Text.Read (readMaybe)
import Pandoc (composerToPandoc)

main :: IO ()
main = do
  args <- getArgs
  case args of
    "server" : _ -> run 9482 app
    "capi-to-org" : filename : _ -> capiToOrg filename
    "set-code-block" : orgFile : _ -> setCodeBlock orgFile
    "get-composer-article" : contentId : filename : _ -> getComposerArticle (Composer.ContentId (Text.pack contentId)) filename
    "refresh-composer-article" : filename : _ -> refreshComposerArticle filename
    _x -> putStrLn ("Unrecognised args: " <> show args)

refreshComposerArticle :: String -> IO ()
refreshComposerArticle filename = do
  composerFile@ComposerFile{contentId} <- readComposerFile filename
  case contentId of
    Nothing -> putStrLn ("Failed to determine contentId from composer file: " <> show composerFile)
    Just cId -> getComposerArticle cId filename

getComposerArticle :: Composer.ContentId -> FilePath -> IO ()
getComposerArticle contentId filename = do
  manager' <- newManager tlsManagerSettings
  pandaCookie <- getEnv "PANDA_COOKIE"

  res <- runClientM
    (ComposerBackend.getContent
      (mkAuthenticatedRequest (Text.pack pandaCookie) ComposerBackend.authenticate)
      contentId
      (Just True)
      (Just False))
    (mkClientEnv manager' (BaseUrl Https "composer.code.dev-gutools.co.uk" 443 ""))

  case res of
    Left err -> do
      putStrLn "Got error:"
      print err
    Right content -> do
      putStrLn "Got content:"
      print content
      pandoc <- composerToPandoc content
      orgText <- toOrg pandoc
      Text.writeFile filename orgText

  where
    toOrg :: Pandoc -> IO Text
    toOrg pandoc = do
      let writerOptions = def { writerWrapText = WrapNone }
      let updateLinks = \case
            l@(Link attrs alt (url, title)) -> case
              Text.stripPrefix "https://www.theguardian.com/" url of
                Just u -> Link attrs alt ("capi-org:" <> u, title)
                Nothing ->
                  if Text.isPrefixOf "profile/" url
                  then Link attrs alt ("capi-org:" <> url, title)
                  else l
            x -> x
      let updatedPandoc = walk updateLinks pandoc
      runIOorExplode (writeOrg writerOptions updatedPandoc)

readComposerFile :: FilePath -> IO ComposerFile
readComposerFile orgFile = do
  orgContents <-  readFile orgFile
  runIOorExplode do
    p@(Pandoc (Meta meta) _blocks) <- readOrg def (Text.pack orgContents)
    let asMetaText :: MetaValue -> Maybe Text
        asMetaText (MetaString s) = Just s
        asMetaText _ = Nothing
    let revisionId :: Maybe Int
        revisionId = Map.lookup "revision_id" meta >>= asMetaText >>= (readMaybe . Text.unpack)
    let contentId = fmap Composer.ContentId (Map.lookup "content_id" meta >>= asMetaText)
    let firstBlockId = fmap Composer.BlockId (Map.lookup "main_block_id" meta >>= asMetaText)
    block <- writeComposer def p
    return ComposerFile{block, revisionId, contentId, firstBlockId}

data ComposerFile = ComposerFile
  { block :: Composer.Block
  , revisionId :: Maybe Int
  , contentId :: Maybe Composer.ContentId
  , firstBlockId :: Maybe Composer.BlockId
  } deriving (Show)

setCodeBlock :: FilePath -> IO ()
setCodeBlock orgFile = do
  ComposerFile
    { block = orgBlock
    , revisionId = rId
    , contentId = Just cId
    , firstBlockId = Just bId
    } <- readComposerFile orgFile
  let Composer.Elements blockElements = orgBlock.elements
  manager' <- newManager tlsManagerSettings
  pandaCookie <- getEnv "PANDA_COOKIE"
  let
    postBlock :: ClientM ComposerBackend.WrappedBlock
    postBlock =
      ComposerBackend.postBlock
        (mkAuthenticatedRequest (Text.pack pandaCookie) ComposerBackend.authenticate)
        -- (fromMaybe (Composer.ContentId "67f7d4468f081771a947ac7c") cId)
        cId
        -- (fromMaybe (Composer.BlockId "67f7d4d28f081771a947ac7d") bId)
        bId
        (Composer.BlockFragment
          { lastModifiedBy = Composer.UserEntity
            { email = "emily.bourke+test@guardian.co.uk"
            , firstName = "Emily (test)"
            , lastName = "Bourke (test)"
            }
          , lastModified = Nothing
          , elements = Just (fmap elementToElementFragment blockElements)
          , attributes = Nothing
          , contributors = []
          , tags = []
          , revisionId = rId
          })
  res <- runClientM
    postBlock
    (mkClientEnv manager' (BaseUrl Https "composer.code.dev-gutools.co.uk" 443 ""))
  case res of
    Left err -> do
      putStrLn "Got error:"
      print err
    Right block -> do
      putStrLn "Got block:"
      print block

capiToOrg :: FilePath -> IO ()
capiToOrg inputFilepath = do
  capiResponse <- eitherDecodeFileStrict inputFilepath
  case capiResponse of
    Left s -> do
      putStrLn "Failed to decode capi response, got error:"
      putStrLn s
    Right (Capi.EndpointWrapper r) -> do
      pandocs <- Reader.responseToPandocs r >>= traverse toOrg
      case pandocs of
        [] -> putStrLn "Error: got no results"
        preamble : rest -> do
          Text.writeFile "preamble.org" preamble
          for_ (zip [1..] rest)
            \(i, result) -> Text.writeFile ("result-" <> show i <> ".org") result
  where
    toOrg :: Pandoc -> IO Text
    toOrg pandoc = do
      let writerOptions = def { writerWrapText = WrapNone }
      let updateLinks = \case
            l@(Link attrs alt (url, title)) -> case
              Text.stripPrefix "https://www.theguardian.com/" url of
                Just u -> Link attrs alt ("capi-org:" <> u, title)
                Nothing ->
                  if Text.isPrefixOf "profile/" url
                  then Link attrs alt ("capi-org:" <> url, title)
                  else l
            x -> x
      let updatedPandoc = walk updateLinks pandoc
      runIOorExplode (writeOrg writerOptions updatedPandoc)

app :: Application
app = serve converterAPI server

converterAPI :: Proxy (ConverterAPI PandocIO)
converterAPI = Proxy

server :: Server (ConverterAPI PandocIO)
server = return "working, hopefully"
  :<|> return "working, hopefully"
  :<|> exampleConversionHandler
  :<|> readCapi

type ConverterAPI m = Get '[PlainText] Text
  :<|> "healthcheck" :> Get '[PlainText] Text
  :<|> "convert"
    :> ReqBody '[PlainText] Text
    :> Post '[PlainText] (Headers '[Servant.Header "Access-Control-Allow-Origin" Text] Text)
    -- assume markdown input and composer output for now
  :<|> "read-capi"
    :> QueryParam "output-format" (TextWriter m)
    :> ReqBody '[JSON] Capi.EndpointWrapper
    :> Post '[PlainText] (Headers '[Servant.Header "Access-Control-Allow-Origin" Text] Text)

newtype TextWriter m = TextWriter {unTextWriter :: WriterOptions -> Pandoc -> m Text}

instance FromHttpApiData (TextWriter PandocIO) where
  parseQueryParam format =
    case lookup format writers of
      Just (Text.Pandoc.TextWriter w) -> Right (TextWriter w)
      Just (Text.Pandoc.ByteStringWriter _) -> Left ("Unsupported writer: " <> format)
      Nothing -> Left ("Unknown writer: " <> format)

readCapi ::
  Maybe (TextWriter PandocIO) ->
  Capi.EndpointWrapper ->
  Handler (Headers '[Servant.Header "Access-Control-Allow-Origin" Text] Text)
readCapi writer (Capi.EndpointWrapper capiResponse) = do
  let pandocWriter = maybe writeMarkdown unTextWriter writer
  let writerOptions = def { writerWrapText = WrapNone }
  pandoc <- liftIO (Reader.responseToPandoc capiResponse)
  let updateLinks = \case
        l@(Link attrs alt (url, title)) -> case
          Text.stripPrefix "https://www.theguardian.com/" url of
            Just u -> Link attrs alt ("capi-org:" <> u, title)
            Nothing ->
              if Text.isPrefixOf "profile/" url
              then Link attrs alt ("capi-org:" <> url, title)
              else l
        x -> x
  let updatedPandoc = walk updateLinks pandoc
  result <- liftIO (runIOorExplode (pandocWriter writerOptions updatedPandoc))
  return (addHeader "*" result)

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

blocksToComposer :: (PandocMonad m) => [Block] -> StateT WriterState m Composer.Block
blocksToComposer = traverse blockToComposer >>> fmap (mconcat >>> makeBlock)
  where
    makeBlock elements =
      Composer.Block -- placeholder data: will probably want to change at some point!
        { elements,
          id = Composer.BlockId "",
          Composer.lastModified = MkSystemTime 0 0,
          dateCreated = MkSystemTime 0 0,
          publishedDate = Nothing,
          firstPublishedDate = Nothing,
          createdBy = Nothing,
          lastModifiedBy = Nothing,
          contributors = [],
          tags = [],
          published = False,
          attributes = Map.empty,
          revisionId = Nothing
        }

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
  Strong _ -> return mempty
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
  Link _attrs altText (url, title) ->
    wrapComposerText
      (\t -> "<a href=\"" <> url <> "\" alt=\"" <> title <> "\">" <> t <> "</a>")
      (fmap mconcat (traverse inlineToComposer altText))
  Image _ _ _ -> return mempty
  Note _ -> return mempty
  Span _ _ -> return mempty
