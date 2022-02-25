module Language.Shape.Stlc.Syntax where

import Data.Symbol

type Program = Block

data Block = Block [Definition] [NeutralTerm] NeutralTerm

data Definition
  = TermDefinition Id Type Term
  | DataDefinition Name Id Id [Constructor]
  -- TODO: add Id for name of recursor, and also make TypeId and TermId types everywhere.

data Constructor = Constructor Id [(Name, Type)]

data BaseType = DataType Id | HoleType Hole [Id]

data Type
  = ArrowType [(Name, Type)] BaseType
  | BaseType BaseType

data Term
  = LambdaTerm [Id] Block -- the names are specified in its `ArrowType`
  | HoleTerm
  | NeutralTerm NeutralTerm
-- All of terms in a hole are of a base type, and they are all neutral forms.
-- Consider having a separate type for neutrals, so reflect that in typing?

data NeutralTerm = Neutral Id [Term]

-- Parameter, Binding, Id

-- A `Binding` appears where an instance of a `Name` is bound, as in `LambdaTerm` and `Case`. The `Name` that is bound is contextually determined, by a `ArrowType` and `Constructor` respectively.
type Name = String -- not necessarily unique
type Id = Symbol -- unique
-- Hole
type Hole = Id