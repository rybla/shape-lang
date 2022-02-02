import Theory (Term(..))

idTerm :: Term
idTerm = Lam "X" (Lam "x" (Var 1))

idType :: Term
idType = Pi "X" U (Pi "x" (Var 0) (Var 0))

test1 :: Term
test1 = App (App idTerm idType) idTerm

type2 :: Term
type2 = Pi "_" U (Pi "X" U U)

test2 :: Term
test2 = App (Lam "A" (Lam "B" (Var 0))) (Lam "X" (Var 0)) 