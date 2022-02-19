module Model where

import Syntax
import Data.Map ( Map )
import Symbol

type Permuation = [Int]

-- Change to an argument list
data Change =
    Output Change |
    Change Type |
    InputChange InputChange

data InputChange = Input Int Change | Insert Int Parameter | Delete Int | Permute Permuation


type Changes = Map Binding (Maybe Change) -- Nothing means deletion, Just change means do that change

-- Why arent these basic function in haskell's standard library?
deleteAt :: [a] -> Int -> [a]
deleteAt xs i = take i xs ++ drop (i + 1) xs

insertAt :: [a] -> Int -> a -> [a]
insertAt xs i x = take i xs ++ [x] ++ drop i xs

applyAt :: [a] -> Int -> (a -> a) -> [a]
applyAt xs i f = take i xs ++ [f (xs !! i)] ++ drop i xs

chType :: Type -> Change -> Type
chType (ArrowType params out) (InputChange (Input i c))
    = ArrowType (applyAt params i (\(name, t) -> (name, chType t c))) out
chType (ArrowType params out) (Output (Change (BaseType b))) = ArrowType params b -- Should only change output to a base type!
chType (ArrowType params out) (InputChange (Insert i t)) = ArrowType (insertAt params i t) out
chType (ArrowType params out) (InputChange (Delete i)) = ArrowType (deleteAt params i) out
chType (ArrowType params out) (InputChange (Permute p)) = undefined
chType _ (Change t) = t
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
chTerm :: Term -> Changes -> Change -> Term
chTerm = undefined

searchTerm :: Changes -> Term -> Term
searchTerm = undefined

-- TODO: reorder args of functions to make mapping easier