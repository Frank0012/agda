{-# LANGUAGE OverlappingInstances,
             GeneralizedNewtypeDeriving #-}

-- | Serialisation of Agda interface files.

-- TODO: It should be easy to produce a decent QuickCheck test suite
-- for this file.

module TypeChecking.Serialise
  ( Binary
  , encode
  , encodeFile
  , decode
  , decodeFile
  , tests
  ) where

import Control.Monad
import Data.Binary hiding (encode, decode, encodeFile, decodeFile)
import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as L
import qualified Codec.Compression.GZip as G

import Syntax.Common
import Syntax.Concrete.Name as C
import Syntax.Abstract.Name as A
import Syntax.Internal
import Syntax.Scope.Base
import Syntax.Position (Position(..), Range)
import qualified Syntax.Position as P
import Syntax.Common
import Syntax.Fixity
import Syntax.Literal

import TypeChecking.Monad

import Utils.Serialise
import Utils.Tuple

-- | Current version of the interface. Only interface files of this version
--   will be parsed.
currentInterfaceVersion :: Int
currentInterfaceVersion = 127

-- | Error message used below.

corruptError :: Monad m => m a
corruptError = fail "Corrupt interface file."

-- | Encodes the input.
--
-- Note that the interface version is stored as the first thing in the
-- resulting string, to ensure that we can always check it; 'decode'
-- takes care of this, and fails if the version does not match.

encode :: Binary a => a -> L.ByteString
encode x =
  B.encode currentInterfaceVersion `L.append`
  G.compress (B.encode x)

-- | Decodes something encoded by 'encode'. Fails with 'error' if the
-- interface version does not match the current interface version.

decode :: Binary a => L.ByteString -> a
decode s
  | v /= currentInterfaceVersion = error "Wrong interface version"
  | otherwise                    = B.decode (G.decompress s')
  where
  (v, s', _) = B.runGetState get s 0

-- | Encodes a file. See 'encode'.

encodeFile :: Binary a => FilePath -> a -> IO ()
encodeFile f x = L.writeFile f (encode x)

-- | Decodes a file written by 'encodeFile'.

decodeFile :: Binary a => FilePath -> IO a
decodeFile f = liftM decode $ L.readFile f

------------------------------------------------------------------------
-- Boring instances
------------------------------------------------------------------------

instance Binary Range where
  put (P.Range a b) = put a >> put b
  get = liftM2 P.Range get get

instance Binary Position where
    put NoPos	     = putWord8 0
    put (Pn f p l c) = putWord8 1 >> put f >> put p >> put l >> put c
    get = do
	tag_ <- getWord8
	case tag_ of
	    0	-> return NoPos
	    1	-> liftM4 Pn get get get get
	    _ -> fail "no parse"

instance Binary C.Name where
    put (C.NoName a b) = putWord8 0 >> put a >> put b
    put (C.Name r xs) = putWord8 1 >> put r >> put xs
    get = {-# SCC "get<C.Name>" #-} do
      tag_ <- getWord8
      case tag_ of
	0 -> liftM2 C.NoName get get
	1 -> liftM2 C.Name get get
	_ -> fail "no parse"

instance Binary NamePart where
  put Hole     = putWord8 0
  put (Id r a) = putWord8 1 >> put r >> put a
  get = {-# SCC "get<NamePart>" #-} do
    tag_ <- getWord8
    case tag_ of
      0 -> return Hole
      1 -> liftM2 Id get get
      _ -> fail "no parse"

instance Binary C.QName where
  put (Qual a b) = putWord8 0 >> put a >> put b
  put (C.QName a) = putWord8 1 >> put a
  get = {-# SCC "get<C.QName>" #-} do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (Qual a b)
      1 -> get >>= \a -> return (C.QName a)
      _ -> fail "no parse"

instance Binary Scope where
  put (Scope a b c) = put a >> put b >> put c
  get = {-# SCC "get<Scope>" #-} get >>= \a -> get >>= \b -> get >>= \c -> return (Scope a b c)

instance Binary Access where
  put PrivateAccess = putWord8 0
  put PublicAccess = putWord8 1
  get = {-# SCC "get<Access>" #-} do
    tag_ <- getWord8
    case tag_ of
      0 -> return PrivateAccess
      1 -> return PublicAccess
      _ -> fail "no parse"

instance Binary NameSpace where
  put (NameSpace a b) = put a >> put b
  get = {-# SCC "get<NameSpace>" #-} get >>= \a -> get >>= \b -> return (NameSpace a b)

instance Binary AbstractName where
  put (AbsName a b) = put a >> put b
  get = {-# SCC "get<AbstractName>" #-} get >>= \a -> get >>= \b -> return (AbsName a b)

instance Binary AbstractModule where
  put (AbsModule a) = put a
  get = {-# SCC "get<AbstractModule>" #-} get >>= \a -> return (AbsModule a)

instance Binary KindOfName where
  put DefName = putWord8 0
  put ConName = putWord8 1
  get = {-# SCC "get<KindOfName>" #-} do
    tag_ <- getWord8
    case tag_ of
      0 -> return DefName
      1 -> return ConName
      _ -> fail "no parse"

instance Binary Syntax.Fixity.Fixity where
  put (LeftAssoc a b) = putWord8 0 >> put a >> put b
  put (RightAssoc a b) = putWord8 1 >> put a >> put b
  put (NonAssoc a b) = putWord8 2 >> put a >> put b
  get = {-# SCC "get<Fixity>" #-} do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (LeftAssoc a b)
      1 -> get >>= \a -> get >>= \b -> return (RightAssoc a b)
      2 -> get >>= \a -> get >>= \b -> return (NonAssoc a b)
      _ -> fail "no parse"

instance Binary A.QName where
  put (A.QName a b) = put a >> put b
  get = {-# SCC "get<A.QName>" #-} get >>= \a -> get >>= \b -> return (A.QName a b)

instance Binary A.ModuleName where
  put (A.MName a) = put a
  get = {-# SCC "get<A.ModuleName>" #-} get >>= \a -> return (A.MName a)

instance Binary A.Name where
  put (A.Name a b c d) = put a >> put b >> put c >> put d
  get = {-# SCC "get<A.Name>" #-} get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (A.Name a b c d)

instance Binary NameId where
  put (NameId a b) = put a >> put b
  get = {-# SCC "get<A.NameId>" #-} get >>= \a -> get >>= \b -> return (NameId a b)

instance Binary Signature where
  put (Sig a b) = put a >> put b
  get = {-# SCC "get<Signature>" #-} get >>= \a -> get >>= \b -> return (Sig a b)

instance Binary Section where
  put (Section a b) = put a >> put b
  get = {-# SCC "get<Section>" #-} get >>= \a -> get >>= \b -> return (Section a b)

instance Binary Telescope where
  put EmptyTel = putWord8 0
  put (ExtendTel a b) = putWord8 1 >> put a >> put b
  get = {-# SCC "get<Telescope>" #-} do
    tag_ <- getWord8
    case tag_ of
      0	-> return EmptyTel
      1 -> get >>= \a -> get >>= \b -> return (ExtendTel a b)
      _ -> fail "no parse"

instance (Binary a) => Binary (Syntax.Common.Arg a) where
  put (Arg a b) = put a >> put b
  get = {-# SCC "get<Arg>" #-} get >>= \a -> get >>= \b -> return (Arg a b)

instance Binary Syntax.Common.Hiding where
  put Hidden = putWord8 0
  put NotHidden = putWord8 1
  get = {-# SCC "get<Hiding>" #-} do
    tag_ <- getWord8
    case tag_ of
      0 -> return Hidden
      1 -> return NotHidden
      _ -> fail "no parse"

instance Binary Syntax.Internal.Type where
  put (El a b) = put a >> put b
  get = {-# SCC "get<Type>" #-} get >>= \a -> get >>= \b -> return (El a b)

instance Binary Syntax.Internal.MetaId where
  put (MetaId a) = put a
  get = {-# SCC "get<MetaId>" #-} get >>= \a -> return (MetaId a)

instance (Binary a) => Binary (Syntax.Internal.Blocked a) where
  put (Blocked a b) = put a >> put b
  get = {-# SCC "get<Blocked>" #-} get >>= \a -> get >>= \b -> return (Blocked a b)

instance (Binary a) => Binary (Syntax.Internal.Abs a) where
  put (Abs a b) = put a >> put b
  get = {-# SCC "get<Abs>" #-} get >>= \a -> get >>= \b -> return (Abs a b)

instance Binary Syntax.Internal.Term where
  put (Var a b) = putWord8 0 >> put a >> put b
  put (Lam a b) = putWord8 1 >> put a >> put b
  put (Lit a) = putWord8 2 >> put a
  put (Def a b) = putWord8 3 >> put a >> put b
  put (Con a b) = putWord8 4 >> put a >> put b
  put (Pi a b) = putWord8 5 >> put a >> put b
  put (Fun a b) = putWord8 6 >> put a >> put b
  put (Sort a) = putWord8 7 >> put a
  put (MetaV a b) = putWord8 8 >> put a >> put b
  put (BlockedV a) = putWord8 9 >> put a
  get = {-# SCC "get<Term>" #-} do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (Var a b)
      1 -> get >>= \a -> get >>= \b -> return (Lam a b)
      2 -> get >>= \a -> return (Lit a)
      3 -> get >>= \a -> get >>= \b -> return (Def a b)
      4 -> get >>= \a -> get >>= \b -> return (Con a b)
      5 -> get >>= \a -> get >>= \b -> return (Pi a b)
      6 -> get >>= \a -> get >>= \b -> return (Fun a b)
      7 -> get >>= \a -> return (Sort a)
      8 -> get >>= \a -> get >>= \b -> return (MetaV a b)
      9 -> get >>= \a -> return (BlockedV a)
      _ -> fail "no parse"

instance Binary Syntax.Internal.Sort where
  put (Type a) = putWord8 0 >> put a
  put Prop = putWord8 1
  put (Lub a b) = putWord8 2 >> put a >> put b
  put (Suc a) = putWord8 3 >> put a
  put (MetaS a) = putWord8 4 >> put a
  get = {-# SCC "get<Sort>" #-} do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (Type a)
      1 -> return Prop
      2 -> get >>= \a -> get >>= \b -> return (Lub a b)
      3 -> get >>= \a -> return (Suc a)
      4 -> get >>= \a -> return (MetaS a)
      _ -> fail "no parse"

instance Binary Syntax.Literal.Literal where
  put (LitInt a b) = putWord8 0 >> put a >> put b
  put (LitFloat a b) = putWord8 1 >> put a >> put b
  put (LitString a b) = putWord8 2 >> put a >> put b
  put (LitChar a b) = putWord8 3 >> put a >> put b
  get = {-# SCC "get<Literal>" #-} do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (LitInt a b)
      1 -> get >>= \a -> get >>= \b -> return (LitFloat a b)
      2 -> get >>= \a -> get >>= \b -> return (LitString a b)
      3 -> get >>= \a -> get >>= \b -> return (LitChar a b)
      _ -> fail "no parse"

instance Binary DisplayForm where
  put NoDisplay = putWord8 0
  put (Display a b c) = putWord8 1 >> put a >> put b >> put c
  get = {-# SCC "get<DisplayForm>" #-} do
    tag_ <- getWord8
    case tag_ of
      0	-> return NoDisplay
      1 -> get >>= \a -> get >>= \b -> get >>= \c -> return (Display a b c)
      _ -> fail "no parse"

instance Binary DisplayTerm where
  put (DTerm a) = putWord8 0 >> put a
  put (DWithApp a b) = putWord8 1 >> put a >> put b
  get = {-# SCC "get<DisplayTerm>" #-} do
    tag_ <- getWord8
    case tag_ of
      0	-> get >>= \a -> return (DTerm a)
      1 -> get >>= \a -> get >>= \b -> return (DWithApp a b)
      _	-> fail "no parse"

instance Binary MutualId where
  put (MutId a) = put a
  get = {-# SCC "get<MutualId>" #-} get >>= \a -> return (MutId a)

instance Binary Definition where
  put (Defn a b c d e) = put a >> put b >> put c >> put d >> put e
  get = {-# SCC "get<Definition>" #-} get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> return (Defn a b c d e)

instance Binary Defn where
  put Axiom		     = putWord8 0
  put (Function a b)	     = putWord8 1 >> put a >> put b
  put (Datatype a b c d e f) = putWord8 2 >> put a >> put b >> put c >> put d >> put e >> put f
  put (Record a b c d e f)   = putWord8 3 >> put a >> put b >> put c >> put d >> put e >> put f
  put (Constructor a b c d)  = putWord8 4 >> put a >> put b >> put c >> put d
  put (Primitive a b c)	     = putWord8 5 >> put a >> put b >> put c
  get = {-# SCC "get<Defn>" #-} do
    tag_ <- getWord8
    case tag_ of
      0 -> return Axiom
      1 -> get >>= \a -> get >>= \b -> return (Function a b)
      2 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> get >>= \f -> return (Datatype a b c d e f)
      3 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> get >>= \f -> return (Record a b c d e f)
      4 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (Constructor a b c d)
      5 -> get >>= \a -> get >>= \b -> get >>= \c -> return (Primitive a b c)
      _ -> fail "no parse"

instance Binary Syntax.Common.IsAbstract where
  put AbstractDef = putWord8 0
  put ConcreteDef = putWord8 1
  get = {-# SCC "get<IsAbstract>" #-} do
    tag_ <- getWord8
    case tag_ of
      0 -> return AbstractDef
      1 -> return ConcreteDef
      _ -> fail "no parse"

instance Binary Syntax.Internal.Clause where
  put (Clause a b) = put a >> put b
  get = {-# SCC "get<Clause>" #-} get >>= \a -> get >>= \b -> return (Clause a b)

instance Binary Syntax.Internal.ClauseBody where
  put (Body a) = putWord8 0 >> put a
  put (Bind a) = putWord8 1 >> put a
  put (NoBind a) = putWord8 2 >> put a
  put NoBody = putWord8 3
  get = {-# SCC "get<ClauseBody>" #-} do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (Body a)
      1 -> get >>= \a -> return (Bind a)
      2 -> get >>= \a -> return (NoBind a)
      3 -> return NoBody
      _ -> fail "no parse"

instance Binary Syntax.Internal.Pattern where
  put (VarP a) = putWord8 0 >> put a
  put (ConP a b) = putWord8 1 >> put a >> put b
  put (LitP a) = putWord8 2 >> put a
  get = {-# SCC "get<Pattern>" #-} do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (VarP a)
      1 -> get >>= \a -> get >>= \b -> return (ConP a b)
      2 -> get >>= \a -> return (LitP a)
      _ -> fail "no parse"

instance Binary a => Binary (Builtin a) where
    put (Prim a) = putWord8 0 >> put a
    put (Builtin a) = putWord8 1 >> put a
    get = {-# SCC "get<Builtin>" #-} do
	tag_ <- getWord8
	case tag_ of
	    0	-> liftM Prim get
	    1	-> liftM Builtin get
	    _ -> fail "no parse"

instance Binary Interface where
    put (Interface a b c d) = put a >> put b >> put c >> put d
    get = {-# SCC "get<Interface>" #-} liftM4 Interface get get get get

------------------------------------------------------------------------
-- All tests
------------------------------------------------------------------------

tests = do
  print (test strings)
  mapM_ (print . test) strings

  where
  test x = decode (encode x) == x

  strings = ["apa", "bepa", "∀ ∃"]
