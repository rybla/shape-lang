module Language.Shape.Stlc.Syntax where

import Prelude
import Prim hiding (Type)
import Data.Symbol
import Data.Tuple
import Data.List
import Data.Map
import Undefined

type Program = Block

data Block = Block (List Definition) Term

data Definition
  = TermDefinition UniqueBinding Type Term
  | DataDefinition UniqueBinding (List Constructor)

data Constructor = Constructor UniqueBinding (List Parameter)

data BaseType = DataType Reference | HoleType Hole

data Type
  = ArrowType (List Parameter) BaseType
  | BaseType BaseType

data Term
  = LambdaTerm (List Binding) Block -- the ids are specified in its `ArrowType`
  | NeutralTerm Reference (List Term)
  | MatchTerm
      Reference -- type of term to match on (must be a DataType)
      Term -- term to match on
      (List Case) -- cases of matched term
  | HoleTerm Hole Weakening Substitution

data Case
  = Case
      Reference -- to the `Constructor`
      (List Binding) -- instances of the `Constructor`'s `Parameter`s
      Block

-- Parameter, Binding, UniqueBinding

-- A `Parameter` appears where the type of a function specifies the name of a `Parameter` and its `Type`, as in `ArrowType` or `Constructor`. No `Id` is specified since this is not an instance of the `Name` as a term. The same `Parameter`'s `Name` could be instantiated multiple times, such as in distinct `LambdaTerm`s and `MatchTerm` cases.
type Parameter = Tuple Name Type

-- A `Binding` appears where an instance of a `Name` is bound, as in `LambdaTerm` and `Case`. The `Name` that is bound is contextually determined, by a `ArrowType` and `Constructor` respectively.
type Binding = Id

-- A `UniqueBinding` appears where a `Name` is introduced and a unique instance of that `Name` is bound at once.
type UniqueBinding = Tuple Name Binding

-- Reference, Name, Id

type Reference = Id

type Name = String -- not necessarily unique

type Id = Int -- Symbol -- unique

-- Hole

type Hole = Int -- Symbol

freshHole :: Unit -> Term
freshHole = undefined

-- Weakening & Substitution

data Weakening -- TODO

data Substitution -- TODO
