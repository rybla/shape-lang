import { List } from "immutable"

export type Context = List<[Label, Term , undefined | Term]>

export type Term
  = {case: "uni"}
  | {case: "pi", label: Label, dom: Term, cod: Term}
  | {case: "lam", label: Label, dom: Term, bod: Term}
  | {case: "let", label: Label, dom: Term, arg: Term, bod: Term}
  | {case: "app", app: Term, arg: Term}
  | {case: "var", dbl: Dbl}
  | {case: "hole", id: HoleId, subs : List<Term>, weak : number}

// A term index specifies a node in a Term AST.
export type TermIx = {} // TODO

// DeBruijn level
export type Dbl = number;

export type Label = {value: string};

export type HoleId = Symbol;