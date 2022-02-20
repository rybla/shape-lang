module Model where

import Syntax
import Data.Map ( Map )
import Symbol

type Permutation = [Int]

data Change = VariableTypeChange TypeChange | VariableDeletion | DataTypeDeletion
    | DataTypeChange DataChange -- figure out

-- Change to an argument list
data TypeChange =
    Output TypeChange |
    TypeReplace Type |
    InputChange InputChange

data InputChange = Input Int TypeChange | Insert Int Parameter | Delete Int | Permute Permutation

data DataChange = ConstructorPermute Permutation
    | DeleteConstructor Int | NewConstructor Int Constructor

type Changes = Map Binding Change

-- Why arent these basic function in haskell's standard library?
deleteAt :: [a] -> Int -> [a]
deleteAt xs i = take i xs ++ drop (i + 1) xs

insertAt :: [a] -> Int -> a -> [a]
insertAt xs i x = take i xs ++ [x] ++ drop i xs

applyAt :: [a] -> Int -> (a -> a) -> [a]
applyAt xs i f = take i xs ++ [f (xs !! i)] ++ drop i xs

chType :: Type -> TypeChange -> Type
chType (ArrowType params out) (InputChange (Input i c))
    = ArrowType (applyAt params i (\(name, t) -> (name, chType t c))) out
chType (ArrowType params out) (Output (TypeReplace (BaseType b))) = ArrowType params b -- Should only TypeChange output to a base type!
chType (ArrowType params out) (InputChange (Insert i t)) = ArrowType (insertAt params i t) out
chType (ArrowType params out) (InputChange (Delete i)) = ArrowType (deleteAt params i) out
chType (ArrowType params out) (InputChange (Permute p)) = undefined
chType _ (TypeReplace t) = t
chType _ _ = error "shouldn't get here"

-- convert arguments to a function of type T into arguments to a function of type (chType T change)
chArgs :: [Term] -> Changes -> InputChange -> [Term]
chArgs args gamma (Insert i p)
    = insertAt (searchArgs gamma args) i (HoleTerm (newSymbol ()) undefined undefined)
chArgs args gamma (Input i c) = searchArgs gamma (applyAt args i (\t -> chTerm t gamma c))
chArgs args gamma (Delete i) = searchArgs gamma (deleteAt args i)
chArgs args gamma (Permute p) = undefined

searchArgs :: Changes -> [Term] -> [Term]
searchArgs gamma = map (searchTerm gamma)

-- Convert a term of type T into a term of type (chType T change)
chTerm :: Term -> Changes -> TypeChange -> Term
chTerm = undefined
-- chTerm is what will actually introduce stuff into Changes

searchTerm :: Changes -> Term -> Term
searchTerm gamma (LambdaTerm binds block) = undefined
searchTerm gamma (NeutralTerm x args) = undefined
searchTerm gamma (MatchTerm ty t cases) = undefined
searchTerm gamma (HoleTerm h _ _) = undefined

-- TODO: reorder args of functions to make mapping easier
-- TODO: Holes need to have values in them sometimes, e.g. when output is changed