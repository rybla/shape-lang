module Language.Shape.Stlc.Syntax where

import Prelude
import Prim hiding (Type)
import Data.Tuple
import Data.Map
import Data.List
import Undefined

data Module
  = Module (List Definition)

data Definition
  = TermDefinition UniqueTermBinding Type Term
  | DataDefinition UniqueTypeBinding (List Constructor)

data Constructor
  = Constructor UniqueTermBinding (List Parameter)

data Type
  = ArrowType (List Parameter) BaseType
  | BaseType BaseType

data Block
  = Block (List Definition) Term

data BaseType
  = DataType TypeReference
  | HoleType HoleId TypeWeakening

data Term
  = LambdaTerm (List TermBinding) Block -- the TermIds are specified in its `ArrowType`
  | NeutralTerm TermReference (List Term)
  | HoleTerm HoleId

-- Parameter, TermBinding, UniqueTermBinding
-- A `Parameter` appears where the type of a function specifies the `TermLabel` of a `Parameter` and its `Type`, as in `ArrowType` or `Constructor`. No `TermId` is specified since this is not an instance of the `TermLabel` as a term. The same `Parameter`'s `TermLabel` could be instantiated multiple times, such as in distinct `LambdaTerm`s and `MatchTerm` cases.
data Parameter
  = Parameter TermLabel Type

data TermLabel
  = TermLabel TermName

-- TermReference, TermLabel, TermName, TermId
-- A `TermBinding` appears where an instance of a `TermName` is bound, as in `LambdaTerm` and `Case`. The `TermName` that is bound is contextually determined, by a `ArrowType` and `Constructor` respectively.
data TermBinding
  = TermBinding TermId

-- A `UniqueTermBinding` appears where a `TermName` is introduced and a unique instance of that `TermName` is bound at once.
data UniqueTermBinding
  = UniqueTermBinding TermName TermBinding

data TermReference
  = TermReference TermId

-- not necessarily unique
data TermName
  = VarName String
  | ConstrName String

-- unique
type TermId
  = Int

--  UniqueTypeBinding, TypeReference, TypeName
data UniqueTypeBinding
  = UniqueTypeBinding TypeName

data TypeReference
  = TypeReference TypeName

-- must be unique
type TypeName
  = String

-- Hole
type HoleId
  = Int

freshHoleTerm :: Unit -> Term
freshHoleTerm = undefined

-- Weakening & Substitution
type TypeWeakening
  = List TypeName
