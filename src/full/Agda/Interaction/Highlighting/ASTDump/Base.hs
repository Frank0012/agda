
-- | Function for generating highlighted AST

module Agda.Interaction.Highlighting.ASTDump.Base
  ( ASTDumpOptions(..)
  , HtmlHighlight(..)
  , prepareCommonDestinationAssets
  , srcFileOfInterface
  , defaultPageGen
  , MonadLogHtml(logHtml)
  , LogHtmlT
  , runLogHtmlWith
  ) where

import Prelude hiding ((!!), concatMap)

import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans ( MonadIO(..), lift )
import Control.Monad.Trans.Reader ( ReaderT(runReaderT), ask )

import Data.Function ( on )
import Data.Foldable (toList, concatMap)
import Data.Maybe
import qualified Data.IntMap as IntMap
import qualified Data.List   as List
import Data.List.Split (splitWhen, chunksOf)
import Data.Text.Lazy (Text)
import Data.String (fromString)
import qualified Data.Text.Lazy as T

import GHC.Generics (Generic)

import qualified Network.URI.Encode

import System.FilePath
import System.Directory

import Text.Blaze.Html5
    ( preEscapedToHtml
    , toHtml
    , stringValue
    , Html
    , (!)
    , Attribute
    )
import qualified Text.Blaze.Html5 as Html5
import qualified Text.Blaze.Html5.Attributes as Attr
import Text.Blaze.Html.Renderer.Text ( renderHtml )
  -- The imported operator (!) attaches an Attribute to an Html value
  -- The defined operator (!!) attaches a list of such Attributes

import Paths_Agda

import Agda.Interaction.Highlighting.Precise hiding (toList)

import Agda.Syntax.Common
import Agda.Syntax.TopLevelModuleName

import qualified Agda.TypeChecking.Monad as TCM
  ( Interface(..)
  , Definition
  )

import Agda.Utils.Function
import qualified Agda.Utils.IO.UTF8 as UTF8
import Agda.Utils.Pretty

import Agda.Utils.Impossible

-- | The Agda data directory containing the files for the HTML backend.

htmlDataDir :: FilePath
htmlDataDir = "html"

-- | The name of the occurrence-highlighting JS file.

occurrenceHighlightJsFile :: FilePath
occurrenceHighlightJsFile = "highlight-hover.js"

-- | The directive inserted before the rendered code blocks

rstDelimiter :: String
rstDelimiter = ".. raw:: html\n"

-- | The directive inserted before rendered code blocks in org

orgDelimiterStart :: String
orgDelimiterStart = "#+BEGIN_EXPORT html\n<pre class=\"Agda\">\n"

-- | The directive inserted after rendered code blocks in org

orgDelimiterEnd :: String
orgDelimiterEnd = "</pre>\n#+END_EXPORT\n"

-- | Determine how to highlight the file

data HtmlHighlight = HighlightAll | HighlightCode | HighlightAuto
  deriving (Show, Eq, Generic)

instance NFData HtmlHighlight

highlightOnlyCode :: HtmlHighlight -> FileType -> Bool
highlightOnlyCode HighlightAll  _ = False
highlightOnlyCode HighlightCode _ = True
highlightOnlyCode HighlightAuto AgdaFileType = False
highlightOnlyCode HighlightAuto MdFileType   = True
highlightOnlyCode HighlightAuto RstFileType  = True
highlightOnlyCode HighlightAuto OrgFileType  = True
highlightOnlyCode HighlightAuto TexFileType  = False

-- | Determine the generated file extension

highlightedFileExt :: FileType -> String
highlightedFileExt ft =
  case ft of
    AgdaFileType -> "agda-ast"
    MdFileType   -> "md-ast"
    RstFileType  -> "rst-ast"
    TexFileType  -> "tex-ast"
    OrgFileType  -> "org-ast"

-- | Options for AST dump

data ASTDumpOptions = ASTDumpOptions
  { htmlOptDir                  :: FilePath
  } deriving Eq

-- | Internal type bundling the information related to a module source file

data SourceFile = SourceFile
  { _srcFileModuleName :: TopLevelModuleName
  , _srcInterface :: TCM.Interface
  }

-- | Bundle up the highlighting info for a source file

srcFileOfInterface ::
  TopLevelModuleName -> TCM.Interface -> SourceFile
srcFileOfInterface m i = SourceFile m i

-- | Logging during HTML generation

type HtmlLogMessage = String
type HtmlLogAction m = HtmlLogMessage -> m ()

class MonadLogHtml m where
  logHtml :: HtmlLogAction m

type LogHtmlT m = ReaderT (HtmlLogAction m) m

instance Monad m => MonadLogHtml (LogHtmlT m) where
  logHtml message = do
    doLog <- ask
    lift $ doLog message

runLogHtmlWith :: Monad m => HtmlLogAction m -> LogHtmlT m a -> m a
runLogHtmlWith = flip runReaderT

renderSourceFile :: ASTDumpOptions -> SourceFile -> [TCM.Definition] -> Text
renderSourceFile opts (SourceFile moduleName iface) defs =
    page moduleName iface defs

defaultPageGen :: (MonadIO m, MonadLogHtml m) => ASTDumpOptions -> SourceFile -> [TCM.Definition] -> m ()
defaultPageGen opts srcFile@(SourceFile moduleName iface) defs = do
  logHtml $ render $ "Generating AST for"  <+> pretty moduleName <+> ((parens (pretty target)) <> ".")
  writeRenderedHtml sexs target
  where
    ext = highlightedFileExt (TCM.iFileType iface)
    target = (htmlOptDir opts) </> modToFile moduleName ext
    sexs = renderSourceFile opts srcFile defs

prepareCommonDestinationAssets :: MonadIO m => ASTDumpOptions -> m ()
prepareCommonDestinationAssets options = liftIO $ do
  -- There is a default directory given by 'defaultHTMLDir'
  let htmlDir = htmlOptDir options
  createDirectoryIfMissing True htmlDir

-- | Converts module names to the corresponding HTML file names.

modToFile :: TopLevelModuleName -> String -> FilePath
modToFile m ext = Network.URI.Encode.encode $ render (pretty m) <.> ext

-- | Generates a highlighted, hyperlinked version of the given module.

writeRenderedHtml
  :: MonadIO m
  => Text       -- ^ Rendered page
  -> FilePath   -- ^ Output path.
  -> m ()
writeRenderedHtml html target = liftIO $ UTF8.writeTextToFile target html


-- | Attach multiple Attributes

(!!) :: Html -> [Attribute] -> Html
h !! as = h ! mconcat as

-- | Constructs the web page, including headers.

page :: TopLevelModuleName
     -> TCM.Interface
     -> [TCM.Definition]
     -> Text
page moduleName i defs =
    fromString $ (show defs)

-- | Position, Contents, Infomation

type TokenInfo =
  ( Int
  , String
  , Aspects
  )

-- | Constructs token stream ready to print.

tokenStream
     :: Text             -- ^ The contents of the module.
     -> HighlightingInfo -- ^ Highlighting information.
     -> [TokenInfo]
tokenStream contents info =
  map (\cs -> case cs of
          (mi, (pos, _)) : _ ->
            (pos, map (snd . snd) cs, fromMaybe mempty mi)
          [] -> __IMPOSSIBLE__) $
  List.groupBy ((==) `on` fst) $
  zipWith (\pos c -> (IntMap.lookup pos infoMap, (pos, c))) [1..] (T.unpack contents)
  where
  infoMap = toMap info

-- | Constructs the HTML displaying the code.

code :: Bool     -- ^ Whether to generate non-code contents as-is
     -> FileType -- ^ Source file type
     -> [TokenInfo]
     -> Html
code onlyCode fileType = mconcat . if onlyCode
  then case fileType of
         -- Explicitly written all cases, so people
         -- get compile error when adding new file types
         -- when they forget to modify the code here
         RstFileType  -> map mkRst . splitByMarkup
         MdFileType   -> map mkMd . chunksOf 2 . splitByMarkup
         AgdaFileType -> map mkHtml
         -- Any points for using this option?
         TexFileType  -> map mkMd . chunksOf 2 . splitByMarkup
         OrgFileType  -> map mkOrg . splitByMarkup
  else map mkHtml
  where
  trd (_, _, a) = a

  splitByMarkup :: [TokenInfo] -> [[TokenInfo]]
  splitByMarkup = splitWhen $ (== Just Markup) . aspect . trd

  mkHtml :: TokenInfo -> Html
  mkHtml (pos, s, mi) =
    -- Andreas, 2017-06-16, issue #2605:
    -- Do not create anchors for whitespace.
    applyUnless (mi == mempty) (annotate pos mi) $ toHtml s

  -- Proposed in #3373, implemented in #3384
  mkRst :: [TokenInfo] -> Html
  mkRst = mconcat . (toHtml rstDelimiter :) . map go
    where
      go token@(_, s, mi) = if aspect mi == Just Background
        then preEscapedToHtml s
        else mkHtml token

  -- Proposed in #3137, implemented in #3313
  -- Improvement proposed in #3366, implemented in #3367
  mkMd :: [[TokenInfo]] -> Html
  mkMd = mconcat . go
    where
      work token@(_, s, mi) = case aspect mi of
        Just Background -> preEscapedToHtml s
        Just Markup     -> __IMPOSSIBLE__
        _               -> mkHtml token
      go [a, b] = [ mconcat $ work <$> a
                  , Html5.pre ! Attr.class_ "Agda" $ mconcat $ work <$> b
                  ]
      go [a]    = work <$> a
      go _      = __IMPOSSIBLE__

  mkOrg :: [TokenInfo] -> Html
  mkOrg tokens = mconcat $ if containsCode then formatCode else formatNonCode
    where
      containsCode = any ((/= Just Background) . aspect . trd) tokens

      startDelimiter = preEscapedToHtml orgDelimiterStart
      endDelimiter = preEscapedToHtml orgDelimiterEnd

      formatCode = startDelimiter : foldr (\x -> (go x :)) [endDelimiter] tokens
      formatNonCode = map go tokens

      go token@(_, s, mi) = if aspect mi == Just Background
        then preEscapedToHtml s
        else mkHtml token

  -- Put anchors that enable referencing that token.
  -- We put a fail safe numeric anchor (file position) for internal references
  -- (issue #2756), as well as a heuristic name anchor for external references
  -- (issue #2604).
  annotate :: Int -> Aspects -> Html -> Html
  annotate pos mi =
    applyWhen hereAnchor (anchorage nameAttributes mempty <>) . anchorage posAttributes
    where
    -- Warp an anchor (<A> tag) with the given attributes around some HTML.
    anchorage :: [Attribute] -> Html -> Html
    anchorage attrs html = Html5.a html !! attrs

    -- File position anchor (unique, reliable).
    posAttributes :: [Attribute]
    posAttributes = concat
      [ [Attr.id $ stringValue $ show pos ]
      , toList $ link <$> definitionSite mi
      , Attr.class_ (stringValue $ unwords classes) <$ guard (not $ null classes)
      ]

    -- Named anchor (not reliable, but useful in the general case for outside refs).
    nameAttributes :: [Attribute]
    nameAttributes = [ Attr.id $ stringValue $ fromMaybe __IMPOSSIBLE__ $ mDefSiteAnchor ]

    classes = concat
      [ concatMap noteClasses (note mi)
      , otherAspectClasses (toList $ otherAspects mi)
      , concatMap aspectClasses (aspect mi)
      ]

    aspectClasses (Name mKind op) = kindClass ++ opClass
      where
      kindClass = toList $ fmap showKind mKind

      showKind (Constructor Inductive)   = "InductiveConstructor"
      showKind (Constructor CoInductive) = "CoinductiveConstructor"
      showKind k                         = show k

      opClass = ["Operator" | op]
    aspectClasses a = [show a]


    otherAspectClasses = map show

    -- Notes are not included.
    noteClasses _s = []

    -- Should we output a named anchor?
    -- Only if we are at the definition site now (@here@)
    -- and such a pretty named anchor exists (see 'defSiteAnchor').
    hereAnchor      :: Bool
    hereAnchor      = here && isJust mDefSiteAnchor

    mDefinitionSite :: Maybe DefinitionSite
    mDefinitionSite = definitionSite mi

    -- Are we at the definition site now?
    here            :: Bool
    here            = maybe False defSiteHere mDefinitionSite

    mDefSiteAnchor  :: Maybe String
    mDefSiteAnchor  = maybe __IMPOSSIBLE__ defSiteAnchor mDefinitionSite

    link (DefinitionSite m defPos _here _aName) = Attr.href $ stringValue $
      -- If the definition site points to the top of a file,
      -- we drop the anchor part and just link to the file.
      applyUnless (defPos <= 1)
        (++ "#" ++
         Network.URI.Encode.encode (show defPos))
         -- Network.URI.Encode.encode (fromMaybe (show defPos) aName)) -- Named links disabled
        (Network.URI.Encode.encode $ modToFile m "html")
