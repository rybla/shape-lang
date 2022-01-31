import { Context, Dbl, Label, Term } from "./Grammar";
import { List } from "immutable"

/** Define general mappings over terms which have a certian structure */

// type Renaming = List<Dbl>
// type Thinnable <T> = (ren : Renaming) => (t : T) => T



type Mapping <Env , Out> = {
    // th : Thinnable<Env>
    newVar : Env
    uni : Out
    pi : (A : Out, B : (e : Env) => Out) => Out
    lam : (body : (E : Env) => Out) => Out,
    let : (label : Label , dom : Out, arg : Out, bod : (env : Env) => Out) => Out
    app : (o1 : Out, o2 : Out) => Out,
    var : (E : Env) => Out,
    hole : Out
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

function mapTerm <Env , Out> (env : List<Env>, t : Term, mapping : Mapping<Env, Out>) : Out {
    if (t.case == "uni"){
        return mapping.uni; 
    } else if (t.case == "pi"){
        return mapping.pi(mapTerm(env, t.dom, mapping),
            (e) => mapTerm(env.insert(env.size, e), t.cod, mapping));
    } else if (t.case == "lam"){
        return mapping.lam((e) => mapTerm(env.insert(env.size, e), t.bod, mapping));
    } else if (t.case == "let"){
        return mapping.let(t.label,
            mapTerm(env, t.dom, mapping),
            mapTerm(env, t.arg, mapping),
            (e) => mapTerm(env.insert(env.size, e), t.bod, mapping));
    } else if (t.case == "app"){
        return mapping.app(mapTerm(env, t.app, mapping), mapTerm(env, t.arg, mapping));
    } else if (t.case == "var"){
        let v = env.get(t.dbl);
        if (v == undefined){
            throw("this shouldn't happen");
        }
        return mapping.var(v);
    } else if (t.case == "hole"){
        mapping.hole;
    }
}

type Sem = {case : "SU"} | {case : "SPi", dom : Sem, cod : (sem : Sem) => Sem} | any

let check : Mapping<Sem, Sem> = {
    uni : {case : "SU"},
    pi : function(A : Sem, B : (e : Sem) => Sem) : Sem{
        return {case : "SPi", dom : A, cod : (sem : Sem) => B(sem)}
    },
    app : function(e1 : Sem, e2 : Sem){
        return e1(e2);
    },
    newVar : undefined,
    lam : function(body : (E : Sem) => Sem){
        // ????????
    },
    let : undefined,
    hole : undefined
    // pi : (A, B) => {case : "SPi", dom : A, cod : (sem) => undefined}
}

/** Notes to self:
 * The newVar case should input a type. It will be used in lambda, pi, and let. I guess it should really input Type x Maybe Term
 * 
 * However, in lambda, where to get type? how should this work with our plan for holes? lambdas don't always have a type annotation.
 * 
 * Check should input a type? ALso, check should output a term. The check I started writing was only doing eval.
 * 
 * I think that easiest approach of all might be to combine:
 * - Incremental type checking
 * - lambdas are still annotated and holes still have ids and are in a hole context
 * 
 * I should bE able to figure out the correct design by just implementing type checking (on programs with some sort of holes)
 * Dont worry about incremental yet. 
 */

function typecheck(t : Term)