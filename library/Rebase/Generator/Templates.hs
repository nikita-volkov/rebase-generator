module Rebase.Generator.Templates
where

import Rebase.Prelude
import NeatInterpolation
import Rebase.Generator.Model
import qualified Rebase.Data.Text as A


forwardingModule :: ModuleName -> Text
forwardingModule (ModuleName name) =
  [text|
    module Rebase.$name
    (
      module $name
    )
    where

    import $name
  |]

dataList1Module :: Text
dataList1Module =
  [text|
    -- |
    -- This module simply provides a more meaningful name
    -- to "Data.List.NonEmpty".
    module Rebase.Data.List1
    (
      module Data.List.NonEmpty,
      List1,
    )
    where

    import Data.List.NonEmpty

    -- |
    -- A more meaningful name for the non-empty list.
    -- Follows the convention behind such names as 'foldr1'.
    type List1 =
      NonEmpty

  |]

dataListModule :: Text
dataListModule =
  [text|
    module Rebase.Data.List
    (
      module Data.List,
      List,
    )
    where

    import Data.List

    -- |
    -- If you're not a fan of magical or special cases,
    -- you probably have already been looking for this alias.
    type List =
      []

  |]

dataBifunctorModule :: Text
dataBifunctorModule =
  [text|
    module Rebase.Data.Bifunctor
    (
      module Data.Bifunctor,
      mapLeft,
      mapRight,
    )
    where

    import Data.Bifunctor


    -- |
    -- A more meaningful and conflict-free alias for 'first'.
    {-# INLINE mapLeft #-}
    mapLeft :: Bifunctor p => (a -> b) -> p a c -> p b c
    mapLeft =
      first

    -- |
    -- A more meaningful and conflict-free alias for 'second'.
    {-# INLINE mapRight #-}
    mapRight :: Bifunctor p => (b -> c) -> p a b -> p a c
    mapRight =
      second

  |]

dataSemigroupModule :: Text
dataSemigroupModule =
  [text|
    module Rebase.Data.Semigroup
    (
      module Data.Semigroup,
      sappend,
    )
    where

    import Data.Semigroup

    sappend =
      (<>)

  |]

dataTextModule :: Text
dataTextModule =
  [text|
    -- |
    -- Unifies some modules,
    -- which are separated in the original API for unknown reasons.
    module Rebase.Data.Text
    (
      module Data.Text,
      module Data.Text.IO,
      module Data.Text.Encoding,
    )
    where

    import Data.Text
    import Data.Text.IO
    import Data.Text.Encoding

  |]

dataTextLazyModule :: Text
dataTextLazyModule =
  [text|
    -- |
    -- Unifies some modules,
    -- which are separated in the original API for unknown reasons.
    module Rebase.Data.Text.Lazy
    (
      module Data.Text.Lazy,
      module Data.Text.Lazy.IO,
      module Data.Text.Lazy.Encoding,
    )
    where

    import Data.Text.Lazy
    import Data.Text.Lazy.IO
    import Data.Text.Lazy.Encoding

  |]

preludeModule :: Text
preludeModule =
  [text|
    -- |
    -- This module reexports the non-conflicting definitions from
    -- the modules exported by this package,
    -- providing a much more featureful alternative to the standard Prelude.
    --
    -- For details check out the source.
    module Rebase.Prelude
    (
      module Exports,
    )
    where


    -- base-prelude
    -------------------------
    import BasePrelude as Exports hiding (fail, Alt, first, second)

    -- base
    -------------------------
    import Rebase.Data.Functor.Identity as Exports
    import Rebase.Data.Bifunctor as Exports
    import Rebase.Data.List as Exports (List)

    -- profunctors
    -------------------------
    import Rebase.Data.Profunctor.Unsafe as Exports

    -- contravariant
    -------------------------
    import Rebase.Data.Functor.Contravariant as Exports
    import Rebase.Data.Functor.Contravariant.Divisible as Exports

    -- contravariant-extras
    -------------------------
    import Rebase.Contravariant.Extras as Exports

    -- semigroups
    -------------------------
    import Rebase.Data.Semigroup as Exports hiding ((<>), Last(..), First(..))
    import Rebase.Data.List.NonEmpty as Exports (NonEmpty)

    -- semigroupoids
    -------------------------
    import Rebase.Data.Semigroupoid as Exports
    import Rebase.Data.Bifunctor.Apply as Exports
    import Rebase.Data.Functor.Alt as Exports hiding (($>), many, some)
    import Rebase.Data.Functor.Apply as Exports hiding (($>))
    import Rebase.Data.Functor.Bind as Exports hiding (join, ($>))
    import Rebase.Data.Functor.Extend as Exports
    import Rebase.Data.Functor.Plus as Exports hiding (($>), some, many)
    import Rebase.Data.Semigroup.Bifoldable as Exports
    import Rebase.Data.Semigroup.Bitraversable as Exports
    import Rebase.Data.Semigroup.Foldable as Exports
    import Rebase.Data.Semigroup.Traversable as Exports
    import Rebase.Data.Semigroupoid as Exports

    -- deepseq
    -------------------------
    import Rebase.Control.DeepSeq as Exports

    -- transformers
    -------------------------
    import Rebase.Control.Monad.IO.Class as Exports
    import Rebase.Control.Monad.Trans.Class as Exports
    import Rebase.Control.Monad.Trans.Cont as Exports hiding (shift, callCC)
    import Rebase.Control.Monad.Trans.Except as Exports (ExceptT(ExceptT), Except, except, runExcept, runExceptT, mapExcept, mapExceptT, withExcept, withExceptT)
    import Rebase.Control.Monad.Trans.Maybe as Exports
    import Rebase.Control.Monad.Trans.Reader as Exports (Reader, runReader, mapReader, withReader, ReaderT(ReaderT), runReaderT, mapReaderT, withReaderT)
    import Rebase.Control.Monad.Trans.State.Strict as Exports (State, runState, evalState, execState, mapState, withState, StateT(StateT), runStateT, evalStateT, execStateT, mapStateT, withStateT)
    import Rebase.Control.Monad.Trans.Writer.Strict as Exports (Writer, runWriter, execWriter, mapWriter, WriterT(..), execWriterT, mapWriterT)

    -- mtl
    -------------------------
    import Rebase.Control.Monad.Cont.Class as Exports
    import Rebase.Control.Monad.Error.Class as Exports hiding (Error(..))
    import Rebase.Control.Monad.Reader.Class as Exports
    import Rebase.Control.Monad.State.Class as Exports
    import Rebase.Control.Monad.Writer.Class as Exports

    -- either
    -------------------------
    import Rebase.Control.Monad.Trans.Either as Exports hiding (left, right)
    import Rebase.Data.Either.Combinators as Exports hiding (isLeft, isRight, mapLeft, mapRight)
    import Rebase.Data.Either.Validation as Exports

    -- fail
    -------------------------
    import Rebase.Control.Monad.Fail as Exports

    -- hashable
    -------------------------
    import Rebase.Data.Hashable as Exports

    -- containers
    -------------------------
    import Rebase.Data.IntMap.Strict as Exports (IntMap)
    import Rebase.Data.IntSet as Exports (IntSet)
    import Rebase.Data.Map.Strict as Exports (Map)
    import Rebase.Data.Sequence as Exports (Seq)
    import Rebase.Data.Set as Exports (Set)

    -- unordered-containers
    -------------------------
    import Rebase.Data.HashSet as Exports (HashSet)
    import Rebase.Data.HashMap.Strict as Exports (HashMap)

    -- vector
    -------------------------
    import Rebase.Data.Vector as Exports (Vector)

    -- bytestring
    -------------------------
    import Rebase.Data.ByteString as Exports (ByteString)

    -- text
    -------------------------
    import Rebase.Data.Text as Exports (Text)

    -- scientific
    -------------------------
    import Rebase.Data.Scientific as Exports (Scientific)

    -- uuid
    -------------------------
    import Rebase.Data.UUID as Exports (UUID)

    -- dlist
    -------------------------
    import Rebase.Data.DList as Exports (DList)

    -- void
    -------------------------
    import Rebase.Data.Void as Exports
    import Rebase.Data.Void.Unsafe as Exports

    -- time
    -------------------------
    import Rebase.Data.Time as Exports

    -- stm
    -------------------------
    import Rebase.Control.Concurrent.STM as Exports

    -- custom
    -------------------------
    import Rebase.Data.List1 as Exports (List1)

  |]

cabal :: List ModuleName -> Text
cabal moduleNames =
  [text|
    name:
      rebase
    version:
      1.0.7
    synopsis:
      A more progressive alternative to the "base" package
    description:
      This package is intended for those who are tired of keeping
      long lists of dependencies to the same essential libraries in each package
      as well as the endless imports of the same APIs all over again.
      It also supports the modern tendencies in the language.
      .
      To solve those problems this package does the following:
      .
      * Reexport the original APIs under the \"Rebase\" namespace.
      .
      * Export all the possible non-conflicting symbols from the \"Rebase.Prelude\" module.
      .
      * Give priority to the modern practices in the conflicting cases.
      .
      The policy behind the package is only to reexport the non-ambiguous
      and non-controversial APIs, which the community has obviously settled on.
      The package is intended to rapidly evolve with the contribution from the community,
      with the missing features being added with pull-requests.
    homepage:
      https://github.com/nikita-volkov/rebase
    bug-reports:
      https://github.com/nikita-volkov/rebase/issues
    author:
      Nikita Volkov <nikita.y.volkov@mail.ru>
    maintainer:
      Nikita Volkov <nikita.y.volkov@mail.ru>
    copyright:
      (c) 2016, Nikita Volkov
    license:
      MIT
    license-file:
      LICENSE
    build-type:
      Simple
    cabal-version:
      >=1.10

    source-repository head
      type:
        git
      location:
        git://github.com/nikita-volkov/rebase.git

    library
      hs-source-dirs:
        library
      default-extensions:
        NoImplicitPrelude, NoMonomorphismRestriction
      default-language:
        Haskell2010
      exposed-modules:
        $modules
      build-depends:
        -- concurrency:
        stm >= 2 && < 3,
        -- data:
        hashable >= 1 && < 2,
        vector >= 0.10 && < 0.12,
        containers >= 0.5 && < 0.6,
        unordered-containers >= 0.2 && < 0.3,
        bytestring >= 0.10 && < 0.11,
        text >= 1 && < 2,
        scientific >= 0.3 && < 0.4,
        uuid == 1.*,
        dlist >= 0.7 && < 0.9,
        void >= 0.7 && < 0.8,
        time >= 1.5 && < 2,
        -- control:
        bifunctors >= 5 && < 6,
        profunctors >= 5 && < 6,
        contravariant >= 1 && < 2,
        contravariant-extras >= 0.3.2 && < 0.4,
        semigroups >= 0.16 && < 0.19,
        semigroupoids >= 5 && < 6,
        deepseq >= 1.4 && < 2,
        transformers >= 0.4 && < 0.6,
        mtl >= 2.2 && < 3.0,
        either >= 4.4 && < 5,
        fail >= 4.9 && < 5,
        -- general:
        base-prelude >= 0.1 && < 2,
        base >= 4.7 && < 5

  |]
  where
    modules =
      A.intercalate "\n" (map (\(ModuleName x) -> "Rebase." <> x) moduleNames)

readme :: Text
readme =
  [text|

    # About

    This package is intended for those who are tired of keeping
    long lists of dependencies to the same essential libraries in each package
    as well as the endless imports of the same APIs all over again.
    It also supports the modern tendencies in the language.

    To solve those problems this package does the following:

    * Reexport the original APIs under the "Rebase" namespace.

    * Export all the possible non-conflicting symbols from the "Rebase.Prelude" module.

    * Give priority to the modern practices in the conflicting cases.

    The policy behind the package is only to reexport the non-ambiguous
    and non-controversial APIs, which the community has obviously settled on.
    The package is intended to rapidly evolve with the contribution from the community,
    with the missing features being added with pull-requests.

    # Contribution

    The contents of this package are generated using
    [the "rebase-generator" tool](https://github.com/nikita-volkov/rebase-generator). So all extension PRs should be done to that project instead.

  |]
