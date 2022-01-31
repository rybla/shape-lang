import { List, Map } from "immutable";

type Term
  = {case: "uni"}
  | {case: "pi", label: Label, dom: Term, cod: Term}
  | {case: "lam", label: Label, dom: Term, bod: Term}
  | {case: "let", label: Label, dom: Term, arg: Term, bod: Term}
  | {case: "app", app: Term, arg: Term}
  | {case: "var", dbl: Dbl}
  | {case: "hole", id: HoleId, subs : List<Term>, weak : number}
// For example, in a context {x , y , z}, 
// ?[z -> 2][y -> 3][a][b][c]
// is represented by {case: "hole", id: id, subs: [2,3], weak: 3}
// My current hypothesis is that holes are always substituted some number of times starting
// at the end of the context, and then weakened some number of times at the end of the context.
// If these assumptions turn out to be wrong, then this representation will need to be revised.

type Nf
  = {case: "lam", label: Label, dom: Type, bod: Nf}
  | {case: "let", label: Label, dom: Type, arg: Nf, bod: Nf}
  | {case: "hole", id: HoleId}

type Ne
  = {case: "app", app: Ne, arg: Nf}
  | {case: "var", dbl: Dbl}
  | Ne

type Type
  = {case: "uni"}
  | {case: "pi", label: Label, dom: Type, cod: Type}
  | Ne
export type Dbl = number;

export type Label = {value: string};

export type HoleId = Symbol;

function typecheck(holeCtx : Map<HoleId, Type>, Gamma : List<Type>, T : Type, t : Term)
    : [Type, Nf, Map<HoleId, Nf>] | undefined {

}

function norm(T : Type, t : Term) : Nf{

}

function unify(t1 : Term, t2 : Term) : [Term , Map<HoleId, Nf>] | undefined{

}