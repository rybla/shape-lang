module Model where

import Symbol

type Program = Block

data Block = Block [Definition] Term

data Definition
  = TermDefinition Binding Type Term
  | DataDefinition Binding [Constructor]

data Type
  = ArrowType [Name] (Either Reference Hole)
  | DataType Reference
  | HoleType Hole

data Constructor = Constructor Binding [Type]

data Term
  = LambdaTerm [Id] Block
  | NeutralTerm Id [Term]
  | HoleTerm Hole Weakening Substitution

-- Binding & Reference

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
