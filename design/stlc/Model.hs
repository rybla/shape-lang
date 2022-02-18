module Model where

import Symbol

type Program = Block

data Type = ArrowType [(Name, Type)] (Either DataType HoleType) | DataType DataType | HoleType HoleType

type DataType = Symbol

type HoleType = Hole

data Block = Block [Definition] Term

data Definition
  = TermDefinition (Name, Symbol) Type Term
  | DataDefinition (Name, Symbol) [Constructor]

data Constructor = Constructor (Name, Symbol) [Type]

data Term = LambdaTerm [Symbol] Block | NeutralTerm Symbol [Term] | HoleTerm

type HoleTerm = Hole

type Hole = Symbol

type Name = String

--
