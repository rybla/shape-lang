module Theory (Term(..), Debruin, Name, HoleId, Nf(..), Ne(..),
    Type(..), Sem(..), Context, HoleCtx, Sub, Ren, SynHoleSub, eval,
    reify, reflect, normalize) where

import Data.Map

type Debruin = Int
type Name = String
type HoleId = Int
data Term = U | Var Debruin | Pi Name Term Term | Lam Name Term
    | App Term Term | THole Hole
    | Let Name Term Term Term
    deriving (Show, Eq)

data Hole = Hole HoleId (Map Debruin Nf) -- intuitively, (Hole x {x -> a, y -> b}) = ?x [x -> a] [y -> b]
    deriving (Show, Eq)

data Nf = NLam Name Nf | Ne Ne | NLet Nf Type Nf | Type Type
    deriving (Show, Eq)
data Ne = NVar Debruin | NApp Ne Nf | NHole Hole
    deriving (Show, Eq)
-- Alternate:
type Ne2 = (Either Debruin HoleId, [Nf])
data Type = NU | NPi Name Type Type | TNe Ne
    deriving (Show, Eq)

data Sem = SU | SPi Name Sem (Sem -> Sem) | SNe Ne
    | SFun (Sem -> Sem) | SHole HoleId (Map Debruin Sem) {-Semsub-} {-Semweak-}

type Context = [(Type, Maybe Nf)]
type HoleCtx = Map HoleId Type

type Sub = [Sem]
type Ren = [Debruin]

-- type SemHoleSub = Map HoleId (Sub -> Sem)

type SynSub = [Nf] -- should be Nf?

forget :: Nf -> Term
forget = undefined -- TODO

-- evalSub :: SynSub -> Sub -> SemHoleSub -> Sub
-- evalSub sSub sub hsub = Prelude.map (\t -> eval (forget t) sub hsub) sSub

eval :: Term -> Sub -> Sem
eval U sub = SU 
eval (Var x) sub = sub !! x
eval (Pi x a b) sub = SPi x (eval a sub) (\x -> eval b (sub ++ [x])) 
eval (Lam x e) sub = SFun (\a -> eval e (sub ++ [a])) 
eval (THole (Hole x m)) sub = SHole x (fmap (\t -> eval (forget t) sub) m)
eval (Let x e t body) sub
    = eval body (sub ++ [reflect (length sub) (eval t sub) (NVar (length sub))]) 
eval (App e1 e2) sub = 
    let s1 = eval e1 sub in
    let s2 = eval e2 sub in
    case s1 of (SFun f) -> f s2
               _ -> error "shouldn't get here"

castType :: Nf -> Type
castType (Type t) = t
castType _ = error "wasn't type"

reify :: Int -> Sem -> Sem -> Nf
reify ctx (SPi x a b) (SFun f) = NLam x (reify (ctx + 1) (b reflection) (f reflection)) 
    where reflection = reflect ctx a (NVar ctx) -- TODO: need to get level at end of context here!
reify ctx (SPi x a b) _ = error "shouldn't get here"
reify ctx SU SU = Type NU
reify ctx SU (SPi x a b) = Type
    (NPi x (castType (reify ctx SU a))
    (castType (reify (ctx + 1) SU (b (reflect ctx a (NVar ctx)))))) -- again, level
reify ctx _ (SNe e) = Ne e -- type should be either ne or hole or U
reify ctx _ (SHole x m) = Ne (NHole (Hole x undefined)) -- type should be either ne or hole or U
reify ctx _ _ = error "no"
-- reify ctx SU (SNe e) = Ne e
-- reify ctx SU _ = error "shouldn't get here"
-- reify ctx (SNe te) (SNe e) = Ne e
-- reify ctx (SNe te) _ = error "shouldn't get here"
-- reify ctx (SHole x m) (SNe e) = Ne e
-- reify ctx (SHole x m) _ = error "shouldn't get here"
-- reify ctx (SFun f) s = error "shouldn't get here, SFun isn't a type"

-- first argument is types for the substitutions
reifySub :: Int -> [Sem] -> Sub -> SynSub
reifySub ctx ts sub = Prelude.map (\(t, s) -> reify ctx t s) (zip ts sub)

reflect :: Int -> Sem -> Ne -> Sem
reflect ctx (SPi x a b) e = SFun (\x -> reflect ctx (b x) (NApp e (reify ctx a x)))
reflect ctx SU e = SNe e
reflect ctx (SNe te) e = SNe e
reflect ctx (SHole x m) e = SNe e
reflect ctx (SFun f) e = error "function isnt a type"

normalize :: Term -> Term -> Nf
normalize t e = reify 0 (eval t []) (eval e [])

getLeft :: Context -> Ne -> Either Debruin (Either (Name, Nf) HoleId)
getLeft ctx (NApp e1 e2) = getLeft ctx e1
getLeft ctx (NHole (Hole x m)) = undefined -- Right (Right x)
getLeft ctx (NVar x) = case ctx !! x of
    (ty, Nothing) -> Left x
    (ty, Just t) -> Right (Left (undefined , t))

type SynHoleSub = Map HoleId Nf

subHolesNf :: SynHoleSub -> Nf -> Nf
subHolesNf = undefined 
subHolesNe :: SynHoleSub -> Ne -> Ne
subHolesNe = undefined 
subHolesTerm :: SynHoleSub -> Term -> Term
subHolesTerm = undefined 

-- We assume that inputs to unify are well typed of the same type as each other
unifyNf :: Context -> Nf -> Nf -> SynHoleSub
unifyNf ctx (NLam x1 e1) (NLam x2 e2) = undefined 
unifyNf ctx (Ne e1) (Ne e2) = undefined 
unifyNf _ _ _ = undefined 