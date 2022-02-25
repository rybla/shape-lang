module Language.Shape.Stlc.Syntax where

import Data.Symbol

type Program = Block

data Block = Block [Definition] NeutralTerm

data Definition
  = TermDefinition Id Type Term
  | DataDefinition Id [Constructor]

data Constructor = Constructor Id [(Name, Type)]

data BaseType = DataType Id | HoleType Hole [Id]

data Type
  = ArrowType [(Name, Type)] BaseType
  | BaseType BaseType

data Term
  = LambdaTerm [Id] Block -- the names are specified in its `ArrowType`
  | HoleTerm [NeutralTerm]
  | NeutralTerm NeutralTerm
-- All of terms in a hole are of a base type, and they are all neutral forms.
-- Consider having a separate type for neutrals, so reflect that in typing?

data NeutralTerm
  = MatchTerm
      Id -- type of term to match on (must be a DataType)
      Term -- term to match on
      [Case] -- cases of matched term
  | Neutral Id [Term]

data Case
  = Case
      Id -- to the `Constructor`
      [Id] -- instances of the `Constructor`'s `Parameter`s
      Block
      -- really just a lambda term

-- Parameter, Binding, Id

-- A `Binding` appears where an instance of a `Name` is bound, as in `LambdaTerm` and `Case`. The `Name` that is bound is contextually determined, by a `ArrowType` and `Constructor` respectively.
type Name = String -- not necessarily unique
type Id = Symbol -- unique
-- Hole
type Hole = Id