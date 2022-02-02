import Language.Haskell.TH (match)
type Debruin = Int
type Name = String
type HoleId = Int
data Term = U | Var Debruin | Pi Term Term | Lam Name Term
    | App Term Term | Hole HoleId
    | Let Name Term Term Term

data Nf = NLam Name Nf | NHole HoleId | Ne Ne | NLet Nf Type Nf
data Ne = NVar Debruin | NApp Ne Nf
data Type = NU | NPi Type Type

data Sem = SU | SPi Sem (Sem -> Sem) | SNe Ne | SFun (Sem -> Sem)

type Sub = [Sem]
type Ren = [Debruin]

eval :: Term -> Sub -> Sem
eval U sub = undefined 
eval (Var x) sub = undefined
eval (Pi a b) sub = undefined 
eval (Lam x e) sub = undefined 
eval (Hole x) sub = undefined 
eval (Let x e t body) sub = undefined 
eval (App e1 e2) sub = 
    let s1 = eval e1 sub in
    let s2 = eval e2 sub in
    case s1 of (SFun f) -> f s2
               _ -> error "shouldn't get here"
