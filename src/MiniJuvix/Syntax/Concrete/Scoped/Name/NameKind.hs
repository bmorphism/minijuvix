module MiniJuvix.Syntax.Concrete.Scoped.Name.NameKind where

import MiniJuvix.Prelude
import MiniJuvix.Prelude.Pretty
import Prettyprinter.Render.Terminal

data NameKind
  = -- | Constructor name.
    KNameConstructor
  | -- | Name introduced by the inductive keyword.
    KNameInductive
  | -- | Name of a defined function (top level or let/where block).
    KNameFunction
  | -- | A locally bound name (patterns, arguments, etc.).
    KNameLocal
  | -- | An axiom.
    KNameAxiom
  | -- | A local module name.
    KNameLocalModule
  | -- | A top module name.
    KNameTopModule
  deriving stock (Show, Eq)

class HasNameKind a where
  getNameKind :: a -> NameKind

instance HasNameKind NameKind where
  getNameKind = id

instance Pretty NameKind where
  pretty = \case
    KNameConstructor -> "constructor"
    KNameInductive -> "inductive type"
    KNameFunction -> "function"
    KNameLocal -> "variable"
    KNameAxiom -> "axiom"
    KNameLocalModule -> "local module"
    KNameTopModule -> "module"

isLocallyBounded :: HasNameKind a => a -> Bool
isLocallyBounded k = case getNameKind k of
  KNameLocal -> True
  _ -> False

isExprKind :: HasNameKind a => a -> Bool
isExprKind k = case getNameKind k of
  KNameLocalModule -> False
  KNameTopModule -> False
  _ -> True

isModuleKind :: HasNameKind a => a -> Bool
isModuleKind k = case getNameKind k of
  KNameLocalModule -> True
  KNameTopModule -> True
  _ -> False

canBeCompiled :: HasNameKind a => a -> Bool
canBeCompiled k = case getNameKind k of
  KNameConstructor -> True
  KNameInductive -> True
  KNameFunction -> True
  KNameAxiom -> True
  KNameLocal -> False
  KNameLocalModule -> False
  KNameTopModule -> False

canHaveFixity :: HasNameKind a => a -> Bool
canHaveFixity k = case getNameKind k of
  KNameConstructor -> True
  KNameInductive -> True
  KNameFunction -> True
  KNameAxiom -> True
  KNameLocal -> False
  KNameLocalModule -> False
  KNameTopModule -> False

nameKindAnsi :: NameKind -> AnsiStyle
nameKindAnsi k = case k of
  KNameConstructor -> colorDull Magenta
  KNameInductive -> colorDull Green
  KNameAxiom -> colorDull Red
  KNameLocalModule -> color Cyan
  KNameFunction -> colorDull Yellow
  KNameLocal -> mempty
  KNameTopModule -> color Cyan
