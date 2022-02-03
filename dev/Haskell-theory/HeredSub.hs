module HeredSub where

import Data.Map
import qualified Data.Map as Map
import Data.Set
import qualified Control.Monad

type Debruin = Int
type Prefix = Int -- location in context, 0 is right of context, and n+1 is one to the left from n
type Name = String
type HoleId = Int
data Term = U | Var Debruin | Pi Name Term Term | Lam Name Term
    | App Term Term | THole Hole
    | Let Name Term Term Term
    deriving (Show, Eq)

data Hole = Hole HoleId (Map Debruin Nf) (Set Debruin) -- intuitively, (Hole x {x -> a, y -> b}) = ?x [x -> a] [y -> b]
    deriving (Show, Eq)

data Nf = NLam Name Nf | Ne Ne | Type Type
    deriving (Show, Eq)
-- data Ne = NVar Debruin | NApp Ne Nf | NHole Hole
    -- deriving (Show, Eq)
type Ne = (Either Debruin Hole, [Nf])
data Type = NU | NPi Name Type Type | TNe Ne
    deriving (Show, Eq)

-- Nf G T -> NF G A -> Var G A -> Nf G T[thing -> thing]
subNf :: Nf -> Nf -> Debruin -> Nf
subNf (NLam x e) e' i = NLam x (subNf e (weakenNf e' 0) (i + 1))
subNf (Ne e) e' i = subNe e e' i
subNf (Type t) e' i = Type (subType t e' i)

subVar :: Debruin -> Debruin -> Maybe Debruin
subVar x y = case compare x y of
    GT -> Just (x - 1)
    EQ -> Nothing
    LT -> Just x

subNe :: Ne -> Nf -> Debruin -> Nf
subNe (Right h, args) e' i = Ne (Right (hsubHole h e' i), fmap (\e -> subNf e e' i) args)
subNe (Left x, args) e' i = case subVar x i of
    Nothing -> appNf e' args -- the same
    Just x' -> Ne (Left x', fmap (\e -> subNf e e' i) args)

subType :: Type -> Nf -> Debruin -> Type
subType NU e' i = NU
subType (NPi x a b) e' i = NPi x (subType a e' i) (subType b (weakenNf e' 0) (i + 1))
subType (TNe e) e' i = case subNe e e' i of
  Type ty -> ty
  _ -> error "hi"

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Shouldn't happen"

hsubHole :: Hole -> Nf -> Debruin -> Hole
hsubHole (Hole x s w) e' i
    = if Data.Set.member i w -- fromJust is ok here because unless there is a bug it should always return Just, and we do want it to throw an error if there is a bug.
        then Hole x (fmap (\e -> subNf e e' i) s) (Data.Set.map (\x -> fromJust (subVar x i)) (Data.Set.delete i w))
        else Hole x (Map.insert i e' (fmap (\e -> subNf e e' i) s)) (Data.Set.map (\x -> fromJust (subVar x i)) w)

appNf :: Nf -> [Nf] -> Nf
appNf (NLam x e) [] = NLam x e
appNf (NLam x e) (arg : rest) = appNf (subNf e arg 0) rest
appNf (Ne (left, args1)) args2 = Ne (left, args1 ++ args2)
appNf (Type i) args = error "shouldn't happen"

weakenVar :: Debruin -> Prefix -> Debruin
weakenVar x i = if x > i then x - 1 else x
weakenNf :: Nf -> Prefix -> Nf
weakenNf (NLam x e) i = NLam x (weakenNf e (i + 1))
weakenNf (Ne e) i = Ne (weakenNe e i)
weakenNf (Type t) i = Type (weakenType t i)
weakenNe :: Ne -> Prefix -> Ne
weakenNe (Right h , args) i = (Right (weakenHole h i) , fmap (`weakenNf` i) args)
weakenNe (Left x , args) i = (Left (weakenVar x i) , fmap (`weakenNf` i) args)
weakenType :: Type -> Prefix -> Type
weakenType NU i = NU
weakenType (NPi x a b) i = NPi x (weakenType a i) (weakenType b (i + 1))
weakenType (TNe e) i = TNe (weakenNe e i)
weakenHole :: Hole -> Prefix -> Hole
weakenHole (Hole x s w) i = Hole x (fmap (`weakenNf` i) s) (Data.Set.insert 0 (Data.Set.map (`weakenVar` 0) w))

normalize :: Term -> Nf
normalize U = Type NU
normalize (Var i) = Ne (Left i, [])
normalize (Pi x a b) = Type (NPi x (normalizeType a) (normalizeType b))
normalize (Lam x e) = NLam x (normalize e)
normalize (App e1 e2) = appNf (normalize e1) [normalize e2]
normalize (THole h) = Ne (Right h, [])
normalize (Let x arg t body) = subNf (normalize body) (normalize arg) 0

normalizeType :: Term -> Type
normalizeType e = case normalize e of
  Type ty -> ty
  _ -> error "shouldn't happen"

normalizeNe :: Term -> Ne
normalizeNe e = case normalize e of
  Ne e -> e
  _ -> error "shouldn't happen"

forgetNf :: Nf -> Term
forgetNf (NLam x e) = Lam x (forgetNf e)
forgetNf (Ne e) = forgetNe e
forgetNf (Type t) = forgetType t

forgetType :: Type -> Term
forgetType NU = U
forgetType (NPi x a b) = Pi x (forgetType a) (forgetType b)
forgetType (TNe e) = forgetNe e

forgetNe :: Ne -> Term
forgetNe (Left i, args) = Prelude.foldr (\arg e -> App e (forgetNf arg)) (Var i) args
forgetNe (Right h, args) = Prelude.foldr (\arg e -> App e (forgetNf arg)) (THole h) args


test1 :: Term
test1 = App (Lam "x" (Var 0)) (Lam "y" (Var 0))

type HoleSub = Map HoleId Term

combineSub :: HoleSub -> HoleSub -> HoleSub
combineSub sub1 sub2 = Map.union (fmap (hSubTerm sub2) sub1) sub2

-- TODO: should we apply to normal forms? We may want things to normalize when we sub in holes.
hSubTerm :: HoleSub -> Term -> Term
hSubTerm sub U = U
hSubTerm sub (Var i) = Var i
hSubTerm sub (Pi x a b) = Pi x (hSubTerm sub a) (hSubTerm sub b)
hSubTerm sub (Lam x e) = Lam x (hSubTerm sub e)
hSubTerm sub (App e1 e2) = App (hSubTerm sub e1) (hSubTerm sub e2)
hSubTerm sub (THole h) = hSubHole sub h
hSubTerm sub (Let x arg ty body) = Let x (hSubTerm sub arg) (hSubTerm sub ty) (hSubTerm sub body) 

hSubHole :: HoleSub -> Hole -> Term
hSubHole sub (Hole x s w) = Map.findWithDefault (THole (Hole x s w)) x sub

unify :: Term -> Term -> Maybe HoleSub
unify (THole (Hole x s w)) e = Just $ Data.Map.singleton x e
unify e (THole h) = unify (THole h) e
unify U U = Just Map.empty
unify (Var x) (Var y) = if x == y then Just Map.empty else Nothing
unify (Pi x a b) (Pi x' a' b') = do
    Control.Monad.unless (x == x') Nothing
    sub1 <- unify a b
    sub2 <- unify (hSubTerm sub1 b) (hSubTerm sub1 b')
    return $ combineSub sub1 sub2
unify (Lam x e) (Lam x' e') = do
    Control.Monad.unless (x == x') Nothing
    unify e e'
unify (App e1 e2) (App e1' e2') = do
    sub1 <- unify e1 e1'
    sub2 <- unify (hSubTerm sub1 e2) (hSubTerm sub1 e2')
    return $ combineSub sub1 sub2
unify (Let x arg ty body) (Let x' arg' ty' body') = do
    Control.Monad.unless (x == x') Nothing
    sub1 <- unify arg arg'
    sub2 <- unify (hSubTerm sub1 ty) (hSubTerm sub1 ty)
    sub3 <- unify (hSubTerm sub2 (hSubTerm sub1 body)) (hSubTerm sub2 (hSubTerm sub1 body'))
    return $ combineSub sub1 sub2
unify _ _ = Just Map.empty