import { List } from "immutable"

export type Ctx = List<Term>;

export type Term
  = {case: "pi", var: Var, cod: Term}
  | {case: "lam", var: Var, dom: Term, cod: Term}
  | {case: "app", app: Term, arg: Term}
  | {case: "var", var: Var}
  | {case: "hole", hole: Hole}

export type Var = {name: string, dbl: number}

export type Hole = {id: number}