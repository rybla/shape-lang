import { Context, Dbl, Label, Term } from "./Grammar";
import { List } from "immutable"

/** Define general mappings over terms which have a certian structure */

// type Renaming = List<Dbl>
// type Thinnable <T> = (ren : Renaming) => (t : T) => T



type mapping <Env , Out> = {
    // th : Thinnable<Env>
    uni : (Gamma : Context) => Out
    pi : (Gamma : Context) => (A : Out) => (B : (e : Env) => Out) => Out
    lam : (Gamma : Context) => (body : (E : Env) => Out) => Out,
//   | {case: "let", label: Label, dom: Metadata<Env,Out>, arg: Metadata<Env,Out>, bod: Metadata<Env,Out>, val : Out}
    app : (Gamma : Context) => (o1 : Out) => (o2 : Out) => Out,
    var : (Gamma : Context) => (E : Env) => Out,
    hole : (Gamma : Context) => Out
}

type Metadata <Env , Out>
  = {case : "uni" , val : Out}
  | {case: "pi", label: Label, dom: Metadata<Env,Out>, cod: Metadata<Env,Out>, var : Env , val : Out}
  | {case: "lam", label: Label, dom: Metadata<Env,Out>, cod: Metadata<Env,Out>, var : Env , val : Out}
  | {case: "let", label: Label, dom: Metadata<Env,Out>, arg: Metadata<Env,Out>, bod: Metadata<Env,Out>, val : Out}
  | {case: "app", app: Metadata<Env,Out>, arg: Metadata<Env,Out>, val : Out}
  | {case: "var", dbl: Dbl, val : Out}
  | {case: "hole", val : Out}

// The question is how should metadata be stored and updated? When does it decide to recompute metadata?
// Should the syntax transformations keep track of it, or should we diff the trees or something?