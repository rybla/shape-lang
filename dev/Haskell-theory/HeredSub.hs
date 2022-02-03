module HeredSub where

import Data.Map
import qualified Data.Map as Map
import Data.Set

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
hsubNf :: Nf -> Nf -> Debruin -> Nf
hsubNf (NLam x e) e' i = NLam x (hsubNf e (weakenNf e' 0) (i + 1))
hsubNf (Ne e) e' i = hsubNe e e' i
hsubNf (Type t) e' i = Type (hsubType t e' i)

subVar :: Debruin -> Debruin -> Maybe Debruin
subVar x y = case compare x y of
    GT -> Just (x - 1)
    EQ -> Nothing 
    LT -> Just x

hsubNe :: Ne -> Nf -> Debruin -> Nf
hsubNe (Right h, args) e' i = Ne (Right (hsubHole h e' i), fmap (\e -> hsubNf e e' i) args)
hsubNe (Left x, args) e' i = case subVar x i of
    Nothing -> appNf e' args -- the same
    Just x' -> Ne (Left x', fmap (\e -> hsubNf e e' i) args)

hsubType :: Type -> Nf -> Debruin -> Type
hsubType NU e' i = NU
hsubType (NPi x a b) e' i = NPi x (hsubType a e' i) (hsubType b (weakenNf e' 0) (i + 1))
hsubType (TNe e) e' i = case hsubNe e e' i of
  Type ty -> ty
  _ -> error "hi"

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Shouldn't happen"

hsubHole :: Hole -> Nf -> Debruin -> Hole
hsubHole (Hole x s w) e' i
    = if Data.Set.member i w -- fromJust is ok here because unless there is a bug it should always return Just, and we do want it to throw an error if there is a bug.
        then Hole x (fmap (\e -> hsubNf e e' i) s) (Data.Set.map (\x -> fromJust (subVar x i)) (Data.Set.delete i w))
        else Hole x (Map.insert i e' (fmap (\e -> hsubNf e e' i) s)) (Data.Set.map (\x -> fromJust (subVar x i)) w)

appNf :: Nf -> [Nf] -> Nf
appNf (NLam x e) [] = NLam x e
appNf (NLam x e) (arg : rest) = appNf (hsubNf e arg 0) rest
appNf (Ne (left, args1)) args2 = Ne (left, args1 ++ args2)
appNf (Type i) args = error "shouldn't happen"

weakenVar :: Debruin -> Prefix -> Debruin
weakenVar x i = if x > i then x - 1 else x 
weakenNf :: Nf -> Prefix -> Nf
weakenNf (NLam x e) i = NLam x (weakenNf e (i + 1))
weakenNf (Ne e) i = Ne (weakenNe e i)
weakenNf (Type t) i = Type (weakenType t i)
weakenNe :: Ne -> Prefix -> Ne
weakenNe (Right h , args) i = (Right (weakenHole h i) , fmap (\e -> weakenNf e i) args)
weakenNe (Left x , args) i = (Left (weakenVar x i) , fmap (\e -> weakenNf e i) args)
weakenType :: Type -> Prefix -> Type
weakenType NU i = NU
weakenType (NPi x a b) i = NPi x (weakenType a i) (weakenType b (i + 1))
weakenType (TNe e) i = TNe (weakenNe e i) 
weakenHole :: Hole -> Prefix -> Hole
weakenHole (Hole x s w) i = Hole x (fmap (\e -> weakenNf e i) s) (Data.Set.insert 0 (Data.Set.map (\i -> weakenVar i 0) w))

normalize :: Term -> Nf
normalize U = undefined
normalize (Var i) = undefined
normalize (Pi x a b) = undefined
normalize (Lam x e) = undefined
normalize (App e1 e2) = undefined
normalize (THole h) = undefined
normalize (Let x arg type body) = undefined
