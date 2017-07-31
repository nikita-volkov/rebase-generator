module Rebase.Generator
(
  generate,
)
where

import Rebase.Generator.Prelude
import Rebase.Generator.Model
import qualified Rebase.Generator.Templates as A
import qualified System.Directory as B
import qualified System.FilePath as C
import qualified Data.Text as D
import qualified Data.Text.IO as D


generate :: IO ()
generate =
  do
    B.removeDirectoryRecursive "library"
    forM_ modules (uncurry overwriteModule)
    D.writeFile "rebase.cabal" (A.cabal (map fst modules))
    D.writeFile "README.md" A.readme

overwriteModule :: ModuleName -> ModuleContents -> IO ()
overwriteModule (ModuleName moduleNameText) (ModuleContents moduleContentsText) =
  do
    B.createDirectoryIfMissing True directoryPath
    D.writeFile filePath moduleContentsText
  where
    directoryPath =
      C.takeDirectory filePath
    filePath =
      moduleNameText &
      D.replace "." "/" &
      mappend "library/Rebase/" &
      flip mappend ".hs" &
      D.unpack

modules :: [Module]
modules =
  [
    -- custom
    cust "Data.List1" A.dataList1Module,

    -- base, 4.7.0.2
    forw "Control.Applicative",
    forw "Control.Arrow",
    forw "Control.Category",
    forw "Control.Concurrent",
    forw "Control.Concurrent.Chan",
    forw "Control.Concurrent.MVar",
    forw "Control.Concurrent.QSem",
    forw "Control.Concurrent.QSemN",
    forw "Control.Exception",
    forw "Control.Exception.Base",
    forw "Control.Monad",
    forw "Control.Monad.Fix",
    forw "Control.Monad.ST",
    forw "Control.Monad.ST.Lazy",
    forw "Control.Monad.ST.Lazy.Unsafe",
    forw "Control.Monad.ST.Strict",
    forw "Control.Monad.ST.Unsafe",
    forw "Control.Monad.Zip",
    forw "Data.Bits",
    forw "Data.Bool",
    forw "Data.Char",
    forw "Data.Coerce",
    forw "Data.Complex",
    forw "Data.Data",
    forw "Data.Dynamic",
    forw "Data.Either",
    forw "Data.Eq",
    forw "Data.Fixed",
    forw "Data.Foldable",
    forw "Data.Function",
    forw "Data.Functor",
    forw "Data.IORef",
    forw "Data.Int",
    forw "Data.Ix",
    cust "Data.List" A.dataListModule,
    forw "Data.Maybe",
    forw "Data.Monoid",
    forw "Data.Ord",
    forw "Data.Proxy",
    forw "Data.Ratio",
    forw "Data.STRef",
    forw "Data.STRef.Lazy",
    forw "Data.STRef.Strict",
    forw "Data.String",
    forw "Data.Traversable",
    forw "Data.Tuple",
    forw "Data.Type.Bool",
    forw "Data.Type.Coercion",
    forw "Data.Type.Equality",
    forw "Data.Typeable",
    forw "Data.Unique",
    forw "Data.Version",
    forw "Data.Word",
    forw "Debug.Trace",
    forw "Foreign",
    forw "Foreign.C",
    forw "Foreign.C.Error",
    forw "Foreign.C.String",
    forw "Foreign.C.Types",
    forw "Foreign.Concurrent",
    forw "Foreign.ForeignPtr",
    forw "Foreign.ForeignPtr.Unsafe",
    forw "Foreign.Marshal",
    forw "Foreign.Marshal.Alloc",
    forw "Foreign.Marshal.Array",
    forw "Foreign.Marshal.Error",
    forw "Foreign.Marshal.Pool",
    forw "Foreign.Marshal.Unsafe",
    forw "Foreign.Marshal.Utils",
    forw "Foreign.Ptr",
    forw "Foreign.StablePtr",
    forw "Foreign.Storable",
    forw "Numeric",
    cust "Prelude" A.preludeModule,
    forw "System.CPUTime",
    forw "System.Console.GetOpt",
    forw "System.Environment",
    forw "System.Exit",
    forw "System.IO",
    forw "System.IO.Error",
    forw "System.IO.Unsafe",
    forw "System.Info",
    forw "System.Mem",
    forw "System.Mem.StableName",
    forw "System.Mem.Weak",
    forw "System.Posix.Internals",
    forw "System.Posix.Types",
    forw "System.Timeout",
    forw "Text.ParserCombinators.ReadP",
    forw "Text.ParserCombinators.ReadPrec",
    forw "Text.Printf",
    forw "Text.Read",
    forw "Text.Read.Lex",
    forw "Text.Show",
    forw "Text.Show.Functions",
    forw "Unsafe.Coerce",

    -- hashable
    forw "Data.Hashable",

    -- vector, 0.10.11.0
    forw "Data.Vector",
    forw "Data.Vector.Fusion.Stream.Monadic",
    forw "Data.Vector.Fusion.Util",
    forw "Data.Vector.Generic",
    forw "Data.Vector.Generic.Base",
    forw "Data.Vector.Generic.Mutable",
    forw "Data.Vector.Generic.New",
    forw "Data.Vector.Internal.Check",
    forw "Data.Vector.Mutable",
    forw "Data.Vector.Primitive",
    forw "Data.Vector.Primitive.Mutable",
    forw "Data.Vector.Storable",
    forw "Data.Vector.Storable.Internal",
    forw "Data.Vector.Storable.Mutable",
    forw "Data.Vector.Unboxed",
    forw "Data.Vector.Unboxed.Base",
    forw "Data.Vector.Unboxed.Mutable",

    -- unordered-containers
    forw "Data.HashMap.Lazy",
    forw "Data.HashMap.Strict",
    forw "Data.HashSet",

    -- containers, 0.5.6.0
    forw "Data.Graph",
    forw "Data.IntMap",
    forw "Data.IntMap.Lazy",
    forw "Data.IntMap.Strict",
    forw "Data.IntSet",
    forw "Data.Map",
    forw "Data.Map.Lazy",
    forw "Data.Map.Strict",
    forw "Data.Sequence",
    forw "Data.Set",
    forw "Data.Tree",

    -- bytestring
    forw "Data.ByteString",
    forw "Data.ByteString.Builder",
    forw "Data.ByteString.Builder.Extra",
    forw "Data.ByteString.Builder.Internal",
    forw "Data.ByteString.Builder.Prim",
    forw "Data.ByteString.Builder.Prim.Internal",
    forw "Data.ByteString.Char8",
    forw "Data.ByteString.Internal",
    forw "Data.ByteString.Lazy",
    forw "Data.ByteString.Lazy.Builder",
    forw "Data.ByteString.Lazy.Builder.ASCII",
    forw "Data.ByteString.Lazy.Builder.Extras",
    forw "Data.ByteString.Lazy.Char8",
    forw "Data.ByteString.Lazy.Internal",
    forw "Data.ByteString.Short",
    forw "Data.ByteString.Short.Internal",
    forw "Data.ByteString.Unsafe",

    -- text, 1.0.0.0
    cust "Data.Text" A.dataTextModule,
    forw "Data.Text.Array",
    forw "Data.Text.Encoding",
    forw "Data.Text.Encoding.Error",
    forw "Data.Text.Foreign",
    forw "Data.Text.IO",
    forw "Data.Text.Internal",
    cust "Data.Text.Lazy" A.dataTextLazyModule,
    forw "Data.Text.Lazy.Builder",
    forw "Data.Text.Lazy.Builder.Int",
    forw "Data.Text.Lazy.Builder.RealFloat",
    forw "Data.Text.Lazy.Encoding",
    forw "Data.Text.Lazy.IO",
    forw "Data.Text.Lazy.Read",
    forw "Data.Text.Read",
    forw "Data.Text.Unsafe",

    -- time, 1.4
    forw "Data.Time",
    forw "Data.Time.Calendar",
    forw "Data.Time.Calendar.Easter",
    forw "Data.Time.Calendar.Julian",
    forw "Data.Time.Calendar.MonthDay",
    forw "Data.Time.Calendar.OrdinalDate",
    forw "Data.Time.Calendar.WeekDate",
    forw "Data.Time.Clock",
    forw "Data.Time.Clock.POSIX",
    forw "Data.Time.Clock.TAI",
    forw "Data.Time.Format",
    forw "Data.Time.LocalTime",

    -- bifunctors, 5
    forw "Data.Biapplicative",
    forw "Data.Bifoldable",
    cust "Data.Bifunctor" A.dataBifunctorModule,
    forw "Data.Bifunctor.Biff",
    forw "Data.Bifunctor.Clown",
    forw "Data.Bifunctor.Flip",
    forw "Data.Bifunctor.Join",
    forw "Data.Bifunctor.Joker",
    forw "Data.Bifunctor.Product",
    forw "Data.Bifunctor.Tannen",
    forw "Data.Bifunctor.Wrapped",
    forw "Data.Bitraversable",

    -- contravariant, 1.2
    forw "Data.Functor.Contravariant",
    forw "Data.Functor.Contravariant.Compose",
    forw "Data.Functor.Contravariant.Divisible",

    -- profunctors
    forw "Data.Profunctor",
    forw "Data.Profunctor.Unsafe",

    -- semigroups, 0.16
    cust "Data.Semigroup" A.dataSemigroupModule,
    forw "Data.List.NonEmpty",

    -- semigroupoids, 5
    forw "Data.Bifunctor.Apply",
    forw "Data.Functor.Alt",
    forw "Data.Functor.Apply",
    forw "Data.Functor.Bind",
    forw "Data.Functor.Bind.Class",
    forw "Data.Functor.Bind.Trans",
    forw "Data.Functor.Extend",
    forw "Data.Functor.Plus",
    forw "Data.Groupoid",
    forw "Data.Isomorphism",
    forw "Data.Semigroup.Bifoldable",
    forw "Data.Semigroup.Bitraversable",
    forw "Data.Semigroup.Foldable",
    forw "Data.Semigroup.Foldable.Class",
    forw "Data.Semigroup.Traversable",
    forw "Data.Semigroup.Traversable.Class",
    forw "Data.Semigroupoid",
    forw "Data.Semigroupoid.Dual",
    forw "Data.Semigroupoid.Ob",
    forw "Data.Semigroupoid.Static",
    forw "Data.Traversable.Instances",

    -- transformers, 0.4.3.0
    forw "Control.Applicative.Backwards",
    forw "Control.Applicative.Lift",
    forw "Control.Monad.IO.Class",
    forw "Control.Monad.Signatures",
    forw "Control.Monad.Trans.Class",
    forw "Control.Monad.Trans.Cont",
    forw "Control.Monad.Trans.Except",
    forw "Control.Monad.Trans.Identity",
    forw "Control.Monad.Trans.List",
    forw "Control.Monad.Trans.Maybe",
    forw "Control.Monad.Trans.RWS",
    forw "Control.Monad.Trans.RWS.Lazy",
    forw "Control.Monad.Trans.RWS.Strict",
    forw "Control.Monad.Trans.Reader",
    forw "Control.Monad.Trans.State",
    forw "Control.Monad.Trans.State.Lazy",
    forw "Control.Monad.Trans.State.Strict",
    forw "Control.Monad.Trans.Writer",
    forw "Control.Monad.Trans.Writer.Lazy",
    forw "Control.Monad.Trans.Writer.Strict",
    forw "Data.Functor.Classes",
    forw "Data.Functor.Compose",
    forw "Data.Functor.Constant",
    forw "Data.Functor.Identity",
    forw "Data.Functor.Product",
    forw "Data.Functor.Reverse",
    forw "Data.Functor.Sum",

    -- mtl, 2.2.0.1
    forw "Control.Monad.Cont",
    forw "Control.Monad.Cont.Class",
    forw "Control.Monad.Error.Class",
    forw "Control.Monad.Identity",
    forw "Control.Monad.List",
    forw "Control.Monad.RWS",
    forw "Control.Monad.RWS.Class",
    forw "Control.Monad.RWS.Lazy",
    forw "Control.Monad.RWS.Strict",
    forw "Control.Monad.Reader",
    forw "Control.Monad.Reader.Class",
    forw "Control.Monad.State",
    forw "Control.Monad.State.Class",
    forw "Control.Monad.State.Lazy",
    forw "Control.Monad.State.Strict",
    forw "Control.Monad.Trans",
    forw "Control.Monad.Writer",
    forw "Control.Monad.Writer.Class",
    forw "Control.Monad.Writer.Lazy",
    forw "Control.Monad.Writer.Strict",

    -- either, 4.4
    forw "Control.Monad.Trans.Either",
    forw "Data.Either.Combinators",
    forw "Data.Either.Validation",

    -- stm, 2.4.4
    forw "Control.Concurrent.STM",
    forw "Control.Concurrent.STM.TArray",
    forw "Control.Concurrent.STM.TBQueue",
    forw "Control.Concurrent.STM.TChan",
    forw "Control.Concurrent.STM.TMVar",
    forw "Control.Concurrent.STM.TQueue",
    forw "Control.Concurrent.STM.TSem",
    forw "Control.Concurrent.STM.TVar",
    forw "Control.Monad.STM",

    -- scientific, 0.3.4
    forw "Data.Scientific",
    forw "Data.ByteString.Builder.Scientific",
    forw "Data.Text.Lazy.Builder.Scientific",

    -- other
    forw "Data.UUID",
    forw "Data.DList",
    forw "Data.Void",
    forw "Data.Void.Unsafe",
    forw "Contravariant.Extras",
    forw "Control.DeepSeq",
    forw "Control.Monad.Fail"
  ]
  where
    forw =
      liftA2 (,) id contents . ModuleName
      where
        contents name =
          ModuleContents (A.forwardingModule name)
    cust name contents =
      (ModuleName name, ModuleContents contents)



