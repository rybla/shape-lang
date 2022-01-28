import { Context, Dbl, Hole, Label, Term } from "./Grammar";
import { List } from "immutable"

/** Define general mappings over terms which have a certian structure */

type Renaming = List<Dbl>
type Thinnable <T> = (ren : Renaming) => (t : T) => T

type HoleCtx = List<[Label , Term]>

type mapping <Env , Out> = {
    th : Thinnable<Env>
    var : (Gamma : Context) => (E : Env) => Out,
    lambda : (Gamma : Context) => (body : (E : Env) => Out) => Out,
    app : (Gamma : Context) => (o1 : Out) => (o2 : Out) => Out,
    U : (Gamma : Context) => Out,
    Pi : (Gamma : Context) => (A : Out) => (B : (e : Env) => Out) => Out
}

type Metadata <Env , Out>
  = {case: "pi", label: Label, dom: Metadata<Env,Out>, cod: Metadata<Env,Out>, var : Env , val : Out}
  | {case: "lam", label: Label, dom: Metadata<Env,Out>, cod: Metadata<Env,Out>, var : Env , val : Out}
  | {case: "let", label: Label, dom: Metadata<Env,Out>, arg: Metadata<Env,Out>, bod: Metadata<Env,Out>, val : Out}
  | {case: "app", app: Metadata<Env,Out>, arg: Metadata<Env,Out>, val : Out}
  | {case: "var", dbl: Dbl, val : Out}
  | {case: "hole", hole: Hole, val : Out}