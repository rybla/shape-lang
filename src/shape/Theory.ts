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

export type Context = List<[Label, Nf , undefined | Term]>

type Nf
  = {case: "lam", label: Label, dom: Type, bod: Nf}
  | {case: "let", label: Label, dom: Type, arg: Nf, bod: Nf}
  | {case: "hole", id: HoleId, subs : List<Term>, weak : number}
  | {case: "int", left: Nf, right: Nf} // intersection
  | Ne

type Ne
  = {case: "app", app: Ne, arg: Nf}
  | {case: "var", dbl: Dbl}
  | {case: "name", name: Label}
  // In true normal forms, these would not be allowed to be value-bound variables.
  // However, in sort normal forms, they can be value bound variables.
  // IDEA: have a form in the syntax called "named", which given a name x and a term e, (named e1 e2)
  // represents the term e2 but is displayed as e1 unless it needs to be computed to e.
  // So for example, (named (List T) ((X : U) -> X -> (T -> X -> X) -> X))
  // e1 and e2 should always be equivalent, and the idea is that e1 should be a Ne where the var on the left
  // is a let-bound var with a value in the context.

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

////////////////////////////////////////////////////////////////////////////////
// Note that norm outputs a soft normal form, not a true normal form.
// Not sure how to implement exactly, but the intuition is that we keep let-bound
// variables as variables until we need to expand them.
////////////////////////////////////////////////////////////////////////////////
function norm(T : Type, t : Term) : Nf{

}

type Sub = List<Sem>
type HoleSub = Map<HoleId, Sem>
type Sem = {case : "SU"} | {case : "SPi", dom : Sem, cod : (sem : Sem) => Sem}
  | {case : "SNe", ne : Ne}
  | SFun

type SFun = {case : "SFun", f : (sem : Sem) => Sem}
  // | any
function eval(t : Term , sub : Sub, holeSub : HoleSub) : Sem{
    if (t.case == "uni"){
        return {case : "SU"};
    } else if (t.case == "pi"){
        return {
          case : "SPi", dom : eval(t.dom, sub, holeSub),
          cod : (a : Sem) => eval (t.cod, sub.push(a), holeSub)
        }
    } else if (t.case == "lam"){
      return {case : "SFun", f: (a : Sem) => eval(t.bod, sub.push(a), holeSub)};
    } else if (t.case == "let"){
      let realValue = t.arg;
      let nameValue = {case : "var", id : sub.size};
      return eval(t.bod, sub.push("HOLEEEE"), holeSub); // this is where I make a decision about what let-bound variables do.
    } else if (t.case == "app"){
      return (eval(t.app, sub, holeSub) as SFun).f(eval(t.arg, sub, holeSub));
    } else if (t.case == "var"){
      return sub.get(t.dbl) as Sem;
    } else if (t.case == "hole"){
      return holeSub.get(t.id) as Sem;
    }
    throw Error("shouldn't get here");
}

// outputs SOFT normal form
function reify(T : Sem, t : Sem) : Nf{

}

function reflect(T : Sem, e : Ne) : Sem{
  if (T.case = "SU"){
    return e;
  } else if (T.case = "SPi"){
    return (a : Sem) => reflect(T.cod(a), {case: "app", app: e, arg: reify(T.dom, a)});
  } else if (T.case = "SNe"){
  }
  throw Error("shouldn't get here");
}
////////////////////////////////////////////////////////////////////////////////

function unify(t1 : Term, t2 : Term) : [Term , Map<HoleId, Nf>] | undefined{

}

/** TODO:
 * 1) implement renaming and sub (not hered)
 * 2) figure out how those act on holes
 * 3) implement unify where it reduces except when there is a let bound
 *    variable, then it only reduces if possible. So that is held in a ne case
 *    which deals with app and those vars in one bit of logic.
 * 
 *    Since it is normalized other than the let bound vars, we can put it in a sort
 *    of normal form, other than that the var at the left might be let bound
 *    and so could be substituted.
 */