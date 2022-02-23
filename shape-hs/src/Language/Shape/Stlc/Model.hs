{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LambdaCase #-}
module Language.Shape.Stlc.Model where

import Data.Map (Map, lookup, fromList, union)
import Data.Symbol
import Language.Shape.Stlc.Syntax
import Prelude hiding (lookup)
import System.Directory.Internal.Prelude (newEmptyMVar)
import Data.Maybe

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
        mapper Insert = HoleTerm []

searchArgs :: Changes -> [Term] -> [Term]
searchArgs gamma = map (searchTerm gamma)

chTerm :: Changes -> TypeChange -> Term -> Term
-- TODO: need to thread context and type through all these function in order to deal with this undefined.
chTerm gamma (TypeReplace ty) t = HoleTerm undefined {-t but remove lambdas-}
chTerm gamma NoChange t = searchTerm gamma t
chTerm gamma (ArrowChange ics tc) (LambdaTerm syms bl)
  = LambdaTerm (map mapper ics) (chBlock gamma tc bl)
  where mapper :: InputChange -> Id
        mapper (Change tc' n) = syms !! n
        mapper Insert = newSymbol ()
chTerm gamma (ArrowChange ics tc) _ = error "only a lambda should be an arrow type"

termToNeutral :: Changes -> Term -> Maybe NeutralTerm
-- For now, just forgets everything that was in the definitions section of the block. TODO: figure out what it should do.
termToNeutral gamma (LambdaTerm syms (Block _ t))
  = termToNeutral gamma (searchTerm (union deletions (fst gamma), (snd gamma)) t)
  where deletions :: Map Id VarChange
        deletions = fromList (map (\x -> (x, VariableDeletion)) syms)
termToNeutral gamma (HoleTerm tes) = Nothing
termToNeutral gamma (NeutralTerm nt) = Just nt

-- TODO: should term at end of block always be of base type? If so, incorporate into syntax?
-- TODO: should things of base type have different case in Term for variables?
genNeutralFrom :: Id -> Type -> NeutralTerm
genNeutralFrom x (ArrowType inputs out)
  = Neutral  x (map (\_ -> HoleTerm []) inputs)
genNeutralFrom x (BaseType bt) = Neutral x []

searchTerm :: Changes -> Term -> Term
searchTerm gamma (LambdaTerm syms bl) = LambdaTerm syms (searchBlock gamma bl)
searchTerm gamma (HoleTerm buffer)
  = HoleTerm (listAcc (map (searchNeutral gamma) buffer))
searchTerm gamma (NeutralTerm t) = case searchNeutral gamma t of
  Left t2 -> NeutralTerm t2
  Right nts -> HoleTerm nts

listAcc :: [Either a [a]] -> [a]
listAcc [] = []
listAcc ((Left a) : es) = a : listAcc es
listAcc ((Right as) : es) = as ++ listAcc es

-- Returns either new term or contents of buffer
searchNeutral :: Changes -> NeutralTerm -> Either NeutralTerm [NeutralTerm]
searchNeutral gamma (Neutral x args) = case lookup x (fst gamma) of
  Nothing -> Left (Neutral x (searchArgs gamma args))
  Just ch -> case ch of
    -- Variable Deletion case?
    VariableTypeChange tc -> case tc of
      NoChange -> Left (Neutral x (searchArgs gamma args))
      (TypeReplace ty) -> undefined
        $ genNeutralFrom x ty : mapMaybe (termToNeutral gamma) args
      (ArrowChange ics outc) ->
        let newArgs = map (\case
              Change c n -> chTerm gamma c (args !! n)
              Insert -> HoleTerm []) ics
        in case outc of
          TypeReplace ty -> Left (Neutral x newArgs)
          _ -> Right (mapMaybe (termToNeutral gamma) newArgs)

-- chNeutral gamma (MatchTerm sym te cas) = _wF gamma

chBlock :: Changes -> TypeChange -> Block -> Block
chBlock gamma c (Block defs t)
  = Block (map (searchDefinition gamma) defs) (chTerm gamma c t)
searchBlock :: Changes -> Block -> Block
searchBlock gamma (Block defs t)
  = Block (map (searchDefinition gamma) defs) (searchTerm gamma  t)

searchDefinition :: Changes -> Definition -> Definition
searchDefinition gamma (TermDefinition x ty t) = _w15
searchDefinition gamma (DataDefinition x ctrs) = _w16

-- TODO: no need for "search*" because we can just use NoChange!

{-

-- Convert a term of type T into a term of type (chType T change)
chTerm :: Changes -> TypeChange -> Term -> Term
chTerm = undefined
-- chTerm is what will actually introduce stuff into Changes
-- If input Term is a hole, don't do anything to it (except search with gamma)

searchTerm :: Changes -> Term -> Term
searchTerm gamma (LambdaTerm binds block) = undefined
searchTerm gamma (Neutral x args) = case lookup x (fst gamma) of
  Nothing -> Neutral x (searchArgs gamma args)
  Just ch -> case ch of
    (VariableTypeChange tc) -> case tc of
      (Output tc') -> HoleTerm (newSymbol ()) [undefined {-Neutral x args-}]
      (TypeReplace ty) -> HoleTerm (newSymbol ()) [undefined {-Var x-} {-args...-}] -- put args and var in buffer
      (InputChange ic) -> Neutral x (chArgs args gamma ic)
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