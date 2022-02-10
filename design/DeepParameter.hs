module DeepParameter where

import Data.List (intercalate)

data Term = Lam [Type] Term | Type Type

data Type = U | Pi [Type] Type | Neu Neu | Hole

data Neu = Neutral String [Term]

-- add parameter

addPrm :: [Int] -> Type -> (Term, Type) -> (Term, Type)
addPrm [i] beta (Lam prms1 bod) (Pi prms2 cod) =
  ( Pi (insert i beta prms) cod,
    Lam (insert i (Type Hole) prms) bod
  )
addPrm (i : ix) beta (Lam prms1 bod) (Pi prms2 cod) =
  let (bod', cod') = addPrm ix beta 

  ( Pi (replace i () prms1) cod,
    Lam (replace i () prms2) bod
  )

-- addPrmType :: [Int] -> Type -> Type -> Type
-- addPrmType [i] beta (Pi prms cod) =
-- addPrmType (i : ix) beta (Pi prms cod) = Pi prms' cod
--   where
--     prms' = take (i - 1) prms ++ [prm'] ++ drop i prms
--     prm' = addPrmType ix beta (prms !! i)
-- addPrmType _ _ alpha = alpha

insert :: Int -> a -> [a] -> [a]
insert 0 x ys = x : ys
insert i x (y : ys) = y : insert (i - 1) x ys

replace :: Int -> a -> [a] -> [a]
replace 0 x (_ : ys) = x : ys
replace i x (y : ys) = y : replace (i - 1) x ys

-- showing

showSeq :: Show a => [a] -> String
showSeq prms = intercalate " " (show <$> prms)

instance Show Term where
  show (Lam prms bod) = "(λ " ++ showSeq prms ++ " . " ++ show bod ++ ")"
  show (Type typ) = show typ

instance Show Type where
  show U = "U"
  show (Pi prms cod) =
    "(Π " ++ showSeq prms ++ " . " ++ show cod ++ ")"
  show (Neu neu) = show neu
  show Hole = "?"

instance Show Neu where
  show (Neutral x args) =
    if length args == 0
      then x
      else "(" ++ x ++ " " ++ showSeq args ++ ")"
