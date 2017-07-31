module Rebase.Generator.Model
where

import Rebase.Generator.Prelude


newtype ModuleName =
  ModuleName Text

newtype ModuleContents =
  ModuleContents Text

type Module =
  (ModuleName, ModuleContents)
