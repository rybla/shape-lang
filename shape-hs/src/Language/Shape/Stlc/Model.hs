module Language.Shape.Stlc.Model where

import Data.Map (Map, lookup, fromList)
import Data.Symbol
import Language.Shape.Stlc.Syntax
import Prelude hiding (lookup)
import System.Directory.Internal.Prelude (newEmptyMVar)

data TypeChange = TypeReplace Type | NoChange | ArrowChange [InputChange] TypeChange
-- data BaseTypeChange = TypeReplace Type | NoChange
data InputChange = Change TypeChange Int | Insert

-- Example:
-- (A,B,C)->D
-- (A,C,?)->D
-- ArrowChange [Change NoChange 0, Change NoChange 2, Insert] NoChange


data DataConstructorChange =  -- TODO: rethink this
    InputsChange [InputChange] |
    DeleteConstructorChange Int | NewConstructor Int Constructor | DeleteConstructor Int

data VarChange = VariableTypeChange TypeChange | VariableDeletion
data DataChange = DataTypeDeletion | DataTypeChange DataConstructorChange

type Changes = (Map Binding VarChange, Map Binding DataChange)

-- Why arent these basic function in haskell's standard library?
deleteAt :: [a] -> Int -> [a]
deleteAt xs i = take i xs ++ drop (i + 1) xs

insertAt :: [a] -> Int -> a -> [a]
insertAt xs i x = take i xs ++ [x] ++ drop i xs

applyAt :: [a] -> Int -> (a -> a) -> [a]
applyAt xs i f = take i xs ++ [f (xs !! i)] ++ drop (i + 1) xs

chType :: TypeChange -> Type -> Type
chType NoChange t = t
chType (TypeReplace t2) t1 = t2
chType (ArrowChange ics out) (ArrowType as b)
  = ArrowType (map mapper ics) bNew
  where bNew = case out of
          TypeReplace (BaseType t) -> t
          NoChange -> b
          _ -> error "base type is not arrow type"
        mapper :: InputChange -> (Name, Type)
        mapper (Change tc n) = (fst $ as !! n , chType tc (snd $ as !! n))
        mapper Insert = ("_" , BaseType (HoleType (newSymbol ()) []))
chType (ArrowChange _ _) (BaseType bt) = error "Can't ArrowChange a base type"

chArgs :: Changes -> [InputChange] -> [Term] -> [Term]
chArgs gamma c args = map mapper c
  where mapper :: InputChange -> Term
        mapper (Change tc n) = searchTerm gamma $ chTerm gamma tc (args !! n) 
        mapper Insert = HoleTerm (newSymbol ()) []

searchArgs :: Changes -> [Term] -> [Term]
searchArgs gamma = map (searchTerm gamma)

chTerm :: Changes -> TypeChange -> Term -> Term
-- TODO: need to thread context and type through all these function in order to deal with this undefined.
chTerm gamma (TypeReplace ty) t = HoleTerm (newSymbol ()) undefined {-t but remove lambdas-}
chTerm gamma NoChange t = searchTerm gamma t
chTerm gamma (ArrowChange ics tc) (LambdaTerm syms bl)
  = LambdaTerm (map mapper ics) (chBlock gamma tc bl)
  where mapper :: InputChange -> Id
        mapper (Change tc' n) = syms !! n
        mapper Insert = newSymbol ()
chTerm gamma (ArrowChange ics tc) _ = error "only a lambda should be an arrow type"

removeOuterLambda :: Term -> Term
-- For now, just forgets everything that was in the definitions section of the block. TODO: figure out what it should do.
removeOuterLambda (LambdaTerm syms (Block _ t)) = searchTerm (deletions, mempty) t
  where deletions :: Map Id VarChange
        deletions = fromList (map (\x -> (x, VariableDeletion)) syms)
removeOuterLambda t = t

-- TODO: should term at end of block always be of base type? If so, incorporate into syntax?
-- TODO: should things of base type have different case in Term for variables?
genNeutralFrom :: Id -> Type -> Term
genNeutralFrom x (ArrowType inputs out)
  = NeutralTerm  x (map (\_ -> HoleTerm (newSymbol ()) []) inputs)
genNeutralFrom x (BaseType bt) = NeutralTerm x []

searchTerm :: Changes -> Term -> Term
searchTerm gamma (LambdaTerm syms bl) = _wD
searchTerm gamma (NeutralTerm x args) = case lookup x (fst gamma) of
  Nothing -> NeutralTerm x (searchArgs gamma args)
  Just ch -> case ch of
    VariableTypeChange tc -> case tc of
      NoChange -> NeutralTerm x (searchArgs gamma args)
      (TypeReplace ty) -> HoleTerm (newSymbol ()) $ genNeutralFrom x ty : map removeOuterLambda args
      (ArrowChange ics tc') -> _wJ -- If tc' =/= NoChange, need to put into hole.
    VariableDeletion -> HoleTerm (newSymbol ()) (map removeOuterLambda args)
searchTerm gamma (MatchTerm sym te cas) = _wF
searchTerm gamma (HoleTerm sym buffer) = _wG

chBlock :: Changes -> TypeChange -> Block -> Block
chBlock = undefined

searchBlock :: Changes -> Block -> Block
searchBlock = undefined

-- TODO: no need for "search*" because we can just use NoChange!

{-

-- Convert a term of type T into a term of type (chType T change)
chTerm :: Changes -> TypeChange -> Term -> Term
chTerm = undefined
-- chTerm is what will actually introduce stuff into Changes
-- If input Term is a hole, don't do anything to it (except search with gamma)

searchTerm :: Changes -> Term -> Term
searchTerm gamma (LambdaTerm binds block) = undefined
searchTerm gamma (NeutralTerm x args) = case lookup x (fst gamma) of
  Nothing -> NeutralTerm x (searchArgs gamma args)
  Just ch -> case ch of
    (VariableTypeChange tc) -> case tc of
      (Output tc') -> HoleTerm (newSymbol ()) [undefined {-NeutralTerm x args-}]
      (TypeReplace ty) -> HoleTerm (newSymbol ()) [undefined {-Var x-} {-args...-}] -- put args and var in buffer
      (InputChange ic) -> NeutralTerm x (chArgs args gamma ic)
    VariableDeletion -> HoleTerm (newSymbol ()) [undefined {-args...-}] -- put args in buffer
searchTerm gamma (MatchTerm ty t cases) = case lookup ty (snd gamma) of
  Nothing -> undefined -- use searchCases on cases and also searchTerm on t
  Just dc -> case dc of
    DataTypeDeletion -> HoleTerm (newSymbol ()) []
    (DataTypeChange dc') -> undefined -- call chCases?
searchTerm gamma (HoleTerm h buffer) = undefined -- should gamma also have hole substitutions?

-- TODO: reorder args of functions to make mapping easier
-- TODO: Holes need to have values in them sometimes, e.g. when output is changed
--      Whatever we do with buffers/holes/whatever, this should be handled by the model.
--      This is because stuff in a buffer needs to be updated when an argument is added to a 
--      function or whatever.
-}

{-

Note: lambdas should never be in a buffer.
Instead, if you have "f (lam a . e)", and then
f is deleted, "e[\a]" (e with a turned into holes where it is used)
is placed into the buffer. Everything in a buffer is always a
neutral form, and so buffers DON'T need types on things.

-}