module Model where

import Symbol

type Program = Block

data Block = Block [Definition] Term

data Definition
  = TermDefinition (Name, Id) Type Term
  | DataDefinition (Name, Id) [Constructor]

data Constructor = Constructor (Name, Id) [(Name, Type)]

data Type
  = ArrowType [(Name, Type)] (Either Reference Hole)
  | DataType Reference
  | HoleType Hole

data Term
  = LambdaTerm [Id] Block
  | NeutralTerm Id [Term]
  | MatchTerm
      Reference -- type of term to be matched on
      Term -- term to be matched on
      [([Id], Term)] -- cases of matched term
  | HoleTerm Hole Weakening Substitution

-- Reference, Name, Id

type Reference = Id

type Name = String -- not necessarily unique

type Id = Symbol -- unique

-- Hole

type Hole = Id

-- Weakening & Substitution

data Weakening -- TODO

data Substitution -- TODO
