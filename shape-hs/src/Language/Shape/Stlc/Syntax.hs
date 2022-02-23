module Language.Shape.Stlc.Syntax where

import Data.Symbol

type Program = Block

data Block = Block [Definition] Term

data Definition
  = TermDefinition UniqueBinding Type Term
  | DataDefinition UniqueBinding [Constructor]

data Constructor = Constructor UniqueBinding [Parameter]

data BaseType = DataType Reference | HoleType Hole [Id]

data Type
  = ArrowType [Parameter] BaseType
  | BaseType BaseType

data Term
  = LambdaTerm [Binding] Block -- the ids are specified in its `ArrowType`
  | HoleTerm Hole [Term]
  | NeutralTerm NeutralTerm
-- All of terms in a hole are of a base type, and they are all neutral forms.
-- Consider having a separate type for neutrals, so reflect that in typing?

data NeutralTerm
  = MatchTerm
      Reference -- type of term to match on (must be a DataType)
      Term -- term to match on
      [Case] -- cases of matched term
  | Neutral Reference [Term]

data Case
  = Case
      Reference -- to the `Constructor`
      [Binding] -- instances of the `Constructor`'s `Parameter`s
      Block
      -- really just a lambda term

-- Parameter, Binding, UniqueBinding

-- A `Parameter` appears where the type of a function specifies the name of a `Parameter` and its `Type`, as in `ArrowType` or `Constructor`. No `Id` is specified since this is not an instance of the `Name` as a term. The same `Parameter`'s `Name` could be instantiated multiple times, such as in distinct `LambdaTerm`s and `MatchTerm` cases.
type Parameter = (Name, Type)

-- A `Binding` appears where an instance of a `Name` is bound, as in `LambdaTerm` and `Case`. The `Name` that is bound is contextually determined, by a `ArrowType` and `Constructor` respectively.
type Binding = Id

-- A `UniqueBinding` appears where a `Name` is introduced and a unique instance of that `Name` is bound at once.
type UniqueBinding = (Name, Id)

-- Reference, Name, Id

type Reference = Id

type Name = String -- not necessarily unique

type Id = Symbol -- unique

-- Hole

type Hole = Id

-- Weakening & Substitution

data Weakening -- TODO

data Substitution -- TODO
