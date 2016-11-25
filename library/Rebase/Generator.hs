module Rebase.Generator
(
  generate,
)
where

import Rebase.Prelude
import Rebase.Generator.Model
import qualified Rebase.Generator.Templates as A
import qualified System.Directory as B
import qualified System.FilePath as C
import qualified Rebase.Data.Text as D


generate :: IO ()
generate =
  do
    B.removeDirectoryRecursive "library"
    forM_ modules (uncurry overwriteModule)
    D.writeFile "rebase.cabal" (A.cabal (map fst modules))

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

forwardingModule :: ModuleName -> Module
forwardingModule =
  liftA2 (,) id forwardingModuleContents

forwardingModuleContents :: ModuleName -> ModuleContents
forwardingModuleContents name =
  ModuleContents (A.forwardingModule name)

modules :: List Module
modules =
  [
    (ModuleName "Prelude", ModuleContents A.preludeModule),
    (ModuleName "Data.List1", ModuleContents A.dataList1Module),
    forwardingModule (ModuleName "Data.Functor.Identity"),
    (ModuleName "Data.Bifunctor", ModuleContents A.dataBifunctorModule),
    (ModuleName "Data.List", ModuleContents A.dataListModule),
    forwardingModule (ModuleName "Data.Hashable"),
    forwardingModule (ModuleName "Data.IntMap.Strict"),
    forwardingModule (ModuleName "Data.IntSet"),
    forwardingModule (ModuleName "Data.Map.Strict"),
    forwardingModule (ModuleName "Data.Sequence"),
    forwardingModule (ModuleName "Data.Set"),
    forwardingModule (ModuleName "Data.HashMap.Strict"),
    forwardingModule (ModuleName "Data.HashSet"),
    forwardingModule (ModuleName "Data.Vector"),
    forwardingModule (ModuleName "Data.Vector.Mutable"),
    forwardingModule (ModuleName "Data.ByteString"),
    forwardingModule (ModuleName "Data.ByteString.Builder"),
    forwardingModule (ModuleName "Data.ByteString.Char8"),
    forwardingModule (ModuleName "Data.ByteString.Lazy"),
    forwardingModule (ModuleName "Data.ByteString.Lazy.Char8"),
    forwardingModule (ModuleName "Data.ByteString.Short"),
    (ModuleName "Data.Text", ModuleContents A.dataTextModule),
    forwardingModule (ModuleName "Data.Text.Encoding.Error"),
    (ModuleName "Data.Text.Lazy", ModuleContents A.dataTextLazyModule),
    forwardingModule (ModuleName "Data.Text.Lazy.Builder"),
    forwardingModule (ModuleName "Data.Time"),
    forwardingModule (ModuleName "Data.Scientific"),
    forwardingModule (ModuleName "Data.UUID"),
    forwardingModule (ModuleName "Data.DList"),
    forwardingModule (ModuleName "Data.Void"),
    forwardingModule (ModuleName "Data.Void.Unsafe"),
    forwardingModule (ModuleName "Data.Profunctor"),
    forwardingModule (ModuleName "Data.Profunctor.Unsafe"),
    forwardingModule (ModuleName "Data.Functor.Contravariant"),
    forwardingModule (ModuleName "Data.Functor.Contravariant.Divisible"),
    forwardingModule (ModuleName "Contravariant.Extras"),
    (ModuleName "Data.Semigroup", ModuleContents A.dataSemigroupModule),
    forwardingModule (ModuleName "Data.List.NonEmpty"),
    forwardingModule (ModuleName "Data.Bifunctor.Apply"),
    forwardingModule (ModuleName "Data.Functor.Alt"),
    forwardingModule (ModuleName "Data.Functor.Apply"),
    forwardingModule (ModuleName "Data.Functor.Bind"),
    forwardingModule (ModuleName "Data.Functor.Extend"),
    forwardingModule (ModuleName "Data.Functor.Plus"),
    forwardingModule (ModuleName "Data.Semigroup.Bifoldable"),
    forwardingModule (ModuleName "Data.Semigroup.Bitraversable"),
    forwardingModule (ModuleName "Data.Semigroup.Foldable"),
    forwardingModule (ModuleName "Data.Semigroup.Traversable"),
    forwardingModule (ModuleName "Data.Semigroupoid"),
    forwardingModule (ModuleName "Control.DeepSeq"),
    forwardingModule (ModuleName "Control.Monad.IO.Class"),
    forwardingModule (ModuleName "Control.Monad.Trans.Class"),
    forwardingModule (ModuleName "Control.Monad.Trans.Cont"),
    forwardingModule (ModuleName "Control.Monad.Trans.Except"),
    forwardingModule (ModuleName "Control.Monad.Trans.Maybe"),
    forwardingModule (ModuleName "Control.Monad.Trans.Reader"),
    forwardingModule (ModuleName "Control.Monad.Trans.State.Lazy"),
    forwardingModule (ModuleName "Control.Monad.Trans.State.Strict"),
    forwardingModule (ModuleName "Control.Monad.Trans.Writer.Lazy"),
    forwardingModule (ModuleName "Control.Monad.Trans.Writer.Strict"),
    forwardingModule (ModuleName "Control.Monad.Cont.Class"),
    forwardingModule (ModuleName "Control.Monad.Error.Class"),
    forwardingModule (ModuleName "Control.Monad.Reader.Class"),
    forwardingModule (ModuleName "Control.Monad.State.Class"),
    forwardingModule (ModuleName "Control.Monad.Writer.Class"),
    forwardingModule (ModuleName "Control.Monad.Trans.Either"),
    forwardingModule (ModuleName "Data.Either.Combinators"),
    forwardingModule (ModuleName "Data.Either.Validation"),
    forwardingModule (ModuleName "Control.Monad.Fail"),
    forwardingModule (ModuleName "Control.Concurrent.STM")
  ]
