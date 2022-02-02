import Theory (Term(..))

idTerm :: Term
idTerm = Lam "X" (Lam "x" (Var 1))

idType :: Term
idType = Pi "X" U (Pi "x" (Var 0) (Var 0))

test1 :: Term
test1 = App (App idTerm idType) idTerm