module Model where

import Symbol

type Program = Block

data Block = Block [Definition] Term

data Definition
  = TermDefinition (Id, Name) Type Term
  | DataDefinition Id [Constructor]

data Constructor = Constructor (Name, Id) [Parameter]

data Type
  = ArrowType [Parameter] (Either Reference Hole)
  | DataType Reference
  | HoleType Hole

data Term
  = LambdaTerm [Id] Block -- the ids are specified in its `ArrowType`
  | NeutralTerm Reference [Term]
  | MatchTerm
      Reference -- type of term to match on (must be a DataType)
      Term -- term to match on
      [Case] -- cases of matched term
  | HoleTerm Hole Weakening Substitution

data Case
  = Case
      Id -- of the `Constructor`
      [Id] -- instances of the `Constructor`'s `Parameter`s
      Block

-- Parameter

-- A `Parameter` appears where the type of a function specifies the name of a `Parameter` and its `Type`, as in `ArrowType` or `Constructor`. No `Id` is specified since this is not an instance of the `Name` as a term. The same `Parameter`'s `Name` could be instantiated multiple times, such as in distinct `LambdaTerm`s and `MatchTerm` cases.
type Parameter = (Name, Type)

-- Reference, Name, Id

type Reference = Id

type Name = String -- not necessarily unique

type Id = Symbol -- unique

-- Hole

type Hole = Id

-- Weakening & Substitution

data Weakening -- TODO

data Substitution -- TODO
