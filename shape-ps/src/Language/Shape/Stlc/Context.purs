module Language.Shape.Stlc.Context where

import Data.Map
import Language.Shape.Stlc.Syntax
import Prelude
import Data.List as List
import Prim hiding (Type)
import Undefined (undefined)

type Context
  = { typeNames :: Array TypeName
    , termIdType :: Map TermId Type
    , termNameIds :: Map TermName (Array TermId)
    , termIdName :: Map TermId TermName
    }

emptyContext :: Context
emptyContext =
  { typeNames: []
  , termIdType: empty
  , termNameIds: empty
  , termIdName: empty
  }

addTypeName :: TypeName -> Context -> Context
addTypeName = undefined

addTermIdType :: TermId -> Type -> Context -> Context
addTermIdType = undefined

addTermNameId :: TermName -> TermId -> Context -> Context
addTermNameId = undefined

addTermIdName :: TermId -> TermName -> Context -> Context
addTermIdName = undefined

addUniqueTermBinding :: UniqueTermBinding -> Type -> Context -> Context
addUniqueTermBinding = undefined

addTermBindinding :: TermBinding -> Type -> Context -> Context
addTermBindinding = undefined

addUniqueTypeBinding :: UniqueTypeBinding -> Context -> Context
addUniqueTypeBinding = undefined

addDefinitions :: List.List Definition -> Context -> Context
addDefinitions defs gamma = List.foldl f gamma defs
  where
  f gamma (TermDefinition x alpha _) = addUniqueTermBinding x alpha gamma

  f gamma (DataDefinition alpha _) = addUniqueTypeBinding alpha gamma
