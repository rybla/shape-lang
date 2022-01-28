import { List } from "immutable"

export type Context = List<[Label, Term , undefined | Term]>

export type Term
  = {case: "pi", label: Label, dom: Term, cod: Term}
  | {case: "lam", label: Label, dom: Term, cod: Term}
  | {case: "let", label: Label, dom: Term, arg: Term, bod: Term}
  | {case: "app", app: Term, arg: Term}
  | {case: "var", dbl: Dbl}
  | {case: "hole", hole: Hole}

// A term index specifies a node in a Term AST.
export type TermIx = {} // TODO

export type Dbl = number;

export type Label = string;

export type HoleId = number;

export type Hole = {holeId: HoleId}