
-- | Function for generating internal abstract syntax trees as s-expressions

module Agda.Interaction.Highlighting.Sexp.Base
  ( SexpOptions(..)
  , srcFileOfInterface
  , defaultSexpGen
  , prepareOutputDirectory
  , MonadLogSexp(logSexp)
  , LogSexpT
  , runLogSexpWith
  ) where

import Agda.Interaction.Highlighting.Sexp.Sexp

import Data.Int

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

import qualified Agda.Utils.FileName as UFN

import GHC.Generics (Generic)

import Data.Strict.Maybe as M

import qualified Network.URI.Encode

import Data.Sequence

import System.FilePath
import System.Directory

import Paths_Agda

import Agda.Syntax.TopLevelModuleName

import Agda.Syntax.Literal (Literal(..))

import Agda.Syntax.Position as PO

import Agda.Syntax.Common
import Agda.Syntax.Abstract as AS
import Agda.Syntax.Internal as AI
import Agda.Syntax.Internal.Elim as EL

import Agda.Syntax.Abstract.Name
  ( QName(..),
    Name(..),
    ModuleName(..),
    Suffix(..)
  )

import Agda.TypeChecking.Monad as TCM

import Agda.Utils.Function
import qualified Agda.Utils.IO.UTF8 as UTF8
import Agda.Utils.Pretty

import Agda.Utils.Impossible

import Debug.Trace
import Agda.Interaction.Highlighting.Sexp.Sexp (Sexpable(toSexp))

dumpFileExt :: FileType -> String
dumpFileExt ft =
  case ft of
    AgdaFileType -> "agda-sexp"
    MdFileType   -> "md-sexp"
    RstFileType  -> "rst-sexp"
    TexFileType  -> "tex-sexp"
    OrgFileType  -> "org-sexp"

-- | Options for AST dump

data SexpOptions = SexpOptions
  { sexpOptDir                  :: FilePath
  } deriving Eq

-- | Internal type bundling the information related to a module source file

data SourceFile = SourceFile
  { _srcFileModuleName :: TopLevelModuleName
  , _srcInterface :: TCM.Interface
  }

-- | Bundle up the highlighting info for a source file

srcFileOfInterface :: TopLevelModuleName -> TCM.Interface -> SourceFile
srcFileOfInterface m i = SourceFile m i

-- | Logging during AST generation

type SexpLogMessage = String
type SexpLogAction m = SexpLogMessage -> m ()

class MonadLogSexp m where
  logSexp :: SexpLogAction m

type LogSexpT m = ReaderT (SexpLogAction m) m

instance Monad m => MonadLogSexp (LogSexpT m) where
  logSexp message = do
    doLog <- ask
    lift $ doLog message

runLogSexpWith :: Monad m => SexpLogAction m -> LogSexpT m a -> m a
runLogSexpWith = flip runReaderT


renderSourceFile :: TopLevelModuleName -> TCM.Interface -> [TCM.Definition] -> Text
renderSourceFile mdl iface defs =
    toText $ constr "module" (toSexp mdl : map toSexp defs)

defaultSexpGen :: (MonadIO m, MonadLogSexp m) => SexpOptions -> SourceFile -> [TCM.Definition] -> m ()
defaultSexpGen opts (SourceFile moduleName iface) defs = do
  logSexp $ render $ "Generating AST for"  <+> pretty moduleName <+> ((parens (pretty target)) <> ".")
  liftIO $ UTF8.writeTextToFile target sexps
  where
    ext = dumpFileExt (TCM.iFileType iface)
    target = (sexpOptDir opts) </> modToFile moduleName ext
    sexps = renderSourceFile moduleName iface defs

prepareOutputDirectory :: MonadIO m => FilePath -> m ()
prepareOutputDirectory sexpDir = liftIO $ do createDirectoryIfMissing True sexpDir



-- | Converts module names to the corresponding AST file names.

modToFile :: TopLevelModuleName -> String -> FilePath
modToFile m ext = Network.URI.Encode.encode $ render (pretty m) <.> ext

-- | Conversion to S-expressions


-- | Frank Crossley: adding the binding site to the sexp
instance Sexpable Name where
    toSexp n = constr "finalName" [(Atom (T.pack $ prettyShow $ nameConcrete n)), (toSexp (nameBindingSite n))]

instance Sexpable ModuleName where
    toSexp (MName lst) = constr "module-name" $ map toSexp lst

-- | Frank Crossley: ignoring module identifiers
instance Sexpable QName where
    toSexp (QName (MName lst) nam) = constr "name" $ (map toSexp lst ++ [toSexp nam])

instance Sexpable Suffix where
    toSexp NoSuffix = Atom "none"
    toSexp (Suffix k) = Integer k

instance Sexpable Origin where
    toSexp UserWritten = constr "user-written" []
    toSexp Inserted = constr "inserted" []
    toSexp Reflected = constr "reflected" []
    toSexp CaseSplit = constr "case-split" []
    toSexp Substitution = constr "substitution" []

instance Sexpable ProjOrigin where
    toSexp ProjPrefix = constr "user-written" []
    toSexp ProjPostfix = constr "user-written" []
    toSexp ProjSystem = constr "inserted" []

instance Sexpable Hiding where
  toSexp Hidden = constr "hidden" []
  toSexp (Instance _) = constr "instance" []
  toSexp NotHidden = constr "not-hidden" []

instance Sexpable a => Sexpable (EL.Elim' a) where
    toSexp (EL.Apply (Arg (ArgInfo {argInfoHiding=hdn, argInfoOrigin=org}) e)) = constr "arg" [toSexp hdn, toSexp org, toSexp e]
    toSexp (EL.Proj org q) = constr "proj" [toSexp org, toSexp q]
    toSexp (EL.IApply x y r) = constr "interval-arg" [toSexp x, toSexp y, toSexp r]

instance Sexpable a => Sexpable (AI.Abs a) where
    toSexp (AI.Abs n e) = constr "bound" [toSexp n, toSexp e]
    toSexp (NoAbs n e) = constr "anonymous" [toSexp e]

instance Sexpable t => Sexpable (AI.Dom t) where
    toSexp (AI.Dom _ _ _ _ t) = toSexp t

instance Sexpable MetaId where
    toSexp (MetaId u (ModuleNameHash v)) = Cons [toSexp u, toSexp v]

instance Sexpable Literal where
    toSexp (LitNat k) = toSexp k
    toSexp (LitWord64 w) = toSexp w
    toSexp (LitFloat x) = toSexp x
    toSexp (LitString s) = toSexp s
    toSexp (LitChar c) = toSexp [c]
    toSexp (LitQName q) = toSexp q
    toSexp (LitMeta mdl mid) = constr "meta" [toSexp mdl, toSexp mid]

instance Sexpable AI.Term where
    toSexp (AI.Var k es) = constr "var" (Integer (toInteger k) : map toSexp es)
    toSexp (AI.Lam _ a) = constr "lambda" [toSexp a]
    toSexp (AI.Lit lit) = constr "literal" [toSexp lit]
    toSexp (AI.Def q es) = constr "apply" (toSexp q :  map toSexp es)
    toSexp (AI.Con (AI.ConHead q _ _ _) _ es) = constr "constr" (toSexp q : map toSexp es)
    toSexp (AI.Pi (AI.Dom _ _ _ _ t) a) = constr "pi" [toSexp t, toSexp a]
    toSexp (AI.Sort s) = constr "sort" [toSexp s]
    toSexp (AI.Level lvl) = constr "level" [toSexp lvl]
    toSexp (AI.MetaV mid es) = constr "meta" (toSexp mid : map toSexp es)
    toSexp (AI.DontCare e) = constr "irrelevant" [toSexp e]
    toSexp (AI.Dummy s es) = constr "internal" (toSexp s : map toSexp es)

instance Sexpable AI.Type where
    toSexp (AI.El srt typ) = constr "type" [toSexp srt, toSexp typ]

-- | Frank Crossley: return position information 
------------------------------------------------
instance Sexpable (PO.Position' ()) where
    toSexp (PO.Pn _ posPos posLine posCol ) = constr "position" [toSexp (fromIntegral (posPos :: Int32) :: Int) , 
                                                                 toSexp (fromIntegral (posLine :: Int32) :: Int), 
                                                                 toSexp (fromIntegral (posCol :: Int32) :: Int)]

instance Sexpable PO.Range where
    toSexp (PO.NoRange) = constr "noRange" []
    toSexp (PO.Range srcFile seq) = constr "range" [toSexp srcFile, toSexp seq]

instance Sexpable PO.SrcFile where
    toSexp (M.Just rngFile) = constr "srcFile" [toSexp rngFile]
    toSexp M.Nothing        = constr "rngNothing" []

instance Sexpable PO.RangeFile where
    toSexp (PO.RangeFile rangeFilePath rangeFileName) = constr "rngFile" []

instance Sexpable UFN.AbsolutePath where
    toSexp (UFN.AbsolutePath txt) = toSexp txt

instance Sexpable (Seq PO.IntervalWithoutFile) where
    toSexp seq = case viewl seq of
                  EmptyL    -> constr "noInterval" []
                  x :< xs     -> constr "intervalwithoutfile" ([(toSexp x), (toSexp xs)])

instance Sexpable PO.IntervalWithoutFile where
    toSexp (PO.Interval istart iend) = constr "interval" [toSexp istart, toSexp iend]
------------------------------------------------

instance Sexpable t => Sexpable (AI.Level' t) where
    toSexp (AI.Max k lvls) = constr "max" (toSexp k : map toSexp lvls)

instance Sexpable t => Sexpable (AI.PlusLevel' t) where
    toSexp (AI.Plus k t) = constr "plus" [toSexp k, toSexp t]

instance Sexpable AI.Sort where
    toSexp s = constr "sort" [sexpify s]
      where
        sexpify (AI.Type lvl) = constr "sort-set" [toSexp lvl]
        sexpify (AI.Prop lvl) = constr "sort-prop" [toSexp lvl]
        sexpify (AI.Inf _ k) = constr "sort-setω" [toSexp k]
        sexpify (AI.SSet lvl) = constr "sort-sset" [toSexp lvl]
        sexpify AI.SizeUniv = constr "sort-size" []
        sexpify AI.LockUniv = constr "sort-lock" []
        sexpify AI.IntervalUniv = constr "sort-interval" []
        sexpify (AI.PiSort (AI.Dom _ _ _ _ t) s a) = constr "sort-pi" [toSexp t, toSexp s, toSexp a]
        sexpify (AI.FunSort t u) = constr "sort-fun" [toSexp t, toSexp u]
        sexpify (AI.UnivSort srt) = constr "sort-univ" [toSexp srt]
        sexpify (AI.MetaS mid es) = constr "sort-meta" (toSexp mid : map toSexp es)
        sexpify (AI.DefS q es) = constr "sort-def" (toSexp q : map toSexp es)
        sexpify (AI.DummyS s) = constr "sort-dummy" [toSexp s]

instance Sexpable TCM.Definition where
    toSexp d = constr "definition" [ toSexp (TCM.defName d),
                                     toSexp (TCM.defType d),
                                     toSexp (TCM.theDef d)
                                    ]
instance Sexpable TCM.Defn where
    toSexp (TCM.Axiom {}) = constr "axiom" []
    toSexp (TCM.DataOrRecSig {}) = constr "data-or-record" []
    toSexp (TCM.GeneralizableVar) = constr "generalizable-var" []
    toSexp (TCM.AbstractDefn d) = constr "abstract" [toSexp d]
    toSexp (TCM.Function {funClauses=cls}) = constr "function" (map toSexp cls)
    toSexp (TCM.Datatype {dataCons=cns, dataSort=srt}) = constr "data" (toSexp srt : map toSexp cns)
    toSexp (TCM.Record {recFields=fds, recConHead=AI.ConHead{conName=q}}) = constr "record" (toSexp q : map toSexp fds)
    toSexp (TCM.Constructor {conData=d}) = constr "constructor" [toSexp d]
    toSexp (TCM.Primitive {primName=s, primClauses=cls}) = constr "primitive" (toSexp s : map toSexp cls)
    toSexp (TCM.PrimitiveSort {primSortName=q, primSortSort=s}) = constr "sort" [toSexp q, toSexp s]

-- | Frank Crossley: add clauseLHSRange and clauseFullRange
instance Sexpable AI.Clause where
    toSexp (AI.Clause {clauseLHSRange=lhsrng, clauseFullRange=rng, clauseTel=tel, namedClausePats=naps, clauseType=typ, clauseBody=bdy}) =
      constr "clause" [constr "pattern" (map toSexp naps), toSexp lhsrng, toSexp rng, toSexp tel, sexpType typ, sexpBody bdy]
        where sexpBody Data.Maybe.Nothing    = constr "no-body" []
              sexpBody (Data.Maybe.Just bdy) = constr "body" [toSexp bdy]
              
              sexpType Data.Maybe.Nothing = constr "no-type" []
              sexpType (Data.Maybe.Just (Arg _ t)) = constr "type" [toSexp t]

instance Sexpable a => Sexpable (AI.Pattern' a) where
  toSexp (AI.VarP _ x) = toSexp x
  toSexp (AI.DotP _ t)  = constr "dot" [toSexp t]
  toSexp (AI.ConP (AI.ConHead {conName=hd}) _ args) = constr "constructor" (toSexp hd : map toSexp args)
  toSexp (AI.LitP _ lit) = constr "literal" [toSexp lit]
  toSexp (AI.ProjP _ n) = constr "proj" [toSexp n]
  toSexp (AI.IApplyP _ a b x) = constr "interval-apply" [toSexp a, toSexp b, toSexp x]
  toSexp (AI.DefP _ n args) = constr "def" (toSexp n : map toSexp args)

instance Sexpable DBPatVar where
  toSexp (AI.DBPatVar n k) = constr "pattern-var" [toSexp n, toSexp k]

instance Sexpable a => Sexpable (NamedArg a) where
  toSexp (Arg _ (Named {nameOf=Data.Maybe.Nothing, namedThing=x})) = constr "arg-noname" [toSexp x]
  toSexp (Arg _ (Named {nameOf=Data.Maybe.Just n,  namedThing=x})) = constr "arg-name" [toSexp n, toSexp x]

instance Sexpable NamedName where
  toSexp (WithOrigin {woThing=Ranged {rangedThing=s}}) = toSexp s

instance Sexpable AI.Telescope where
    toSexp tel = constr "telescope" $ telescopeToList tel
        where telescopeToList :: AI.Telescope -> [Sexp]
              telescopeToList AI.EmptyTel = []
              telescopeToList (AI.ExtendTel t (Abs n tel)) = (constr "bound" [toSexp n, toSexp t]) : telescopeToList tel
              telescopeToList (AI.ExtendTel t (NoAbs n tel)) = (constr "anonymous" [toSexp n, toSexp t]) : telescopeToList tel

instance Sexpable TopLevelModuleName where
    toSexp (TopLevelModuleName rng (ModuleNameHash id) ps) = constr "module-name" (toSexp id : (map (Atom . T.fromStrict) $ toList ps))
