module Model where

import Symbol

type Program = Block

data Block = Block [Definition] Term

data Definition
  = TermDefinition Binding Type Term
  | DataDefinition Binding [Constructor]

data Type
  = ArrowType [Parameter] (Either Reference Hole)
  | DataType Reference
  | HoleType Hole

data Constructor = Constructor Binding [(Name, Type)]

data Term
  = LambdaTerm [Id] Block
  | NeutralTerm Id [Term]
  | HoleTerm Hole Weakening Substitution
  | MatchTerm Reference [([Reference], Term)] Term
--            ^ the type  ^ args       ^ case ^ term to be matched on

-- Parameter, Binding, Reference

type Parameter = (Name, Type)

type Binding = (Name, Id)

type Reference = Id

-- Name & Id

type Name = String -- not necessarily unique

type Id = Symbol -- unique

-- Hole

type Hole = Id

-- Weakening & Substitution

data Weakening -- TODO

data Substitution -- TODO
