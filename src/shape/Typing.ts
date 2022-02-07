import { List } from "immutable";
import { Context, freshHoleTerm, Term, TermIx, TermLambda, TermLet, TermNeutral, TermPi, TermUniverse } from "./Grammar";

// Checks that `a` has type `alpha`.
export function check(gamma: Context, a: Term, alpha: Term): boolean {
  throw new Error("unimplemented");
}

// Infers the type of `a`.
export function infer(gamma: Context, a: Term): Term {
  switch (a.case) {
    case "universe": return {case: "universe", universelevel: a.universelevel + 1, format: {}};
    case "pi": {
      let domainType = infer(gamma, a.domain) as TermUniverse;
      let codomainType = infer(gamma.push([a.label, a.domain, undefined]), a.codomain) as TermUniverse;
      let universelevel = Math.max(domainType.universelevel, codomainType.universelevel) + 1;
      return {case: "universe", universelevel, format: {}};
    }
    case "lambda": {
      let codomain = infer(gamma.push([a.label, a.domain, undefined]), a.body);
      return {case:"pi", label: a.label, domain: a.domain, codomain, format: {}};
    }
    case "let": {
      return infer(gamma.push([a.label, a.domain, a.argument]), a.body);
    }
    case "neutral": {
      return freshHoleTerm();
    }
  }
}

// Unifies the terms `a1` and `a2`.
export function unify(gamma: Context, a1: Term, a2: Term): Term {
  throw new Error("unimplemented");
}


export function collectContext(a: Term, ix: TermIx, gamma: Context = List()): [Context, Term] {
  let step = ix.first();
  if (step) {
    ix = ix.shift();
    switch (step.case) {
      case "pi domain": {
        a = a as TermPi;
        return collectContext(a.domain, ix, gamma);
      }
      case "pi codomain": {
        a = a as TermPi;
        return collectContext(a.codomain, ix, gamma.push([a.label, a.domain, undefined]));
      }
      case "lambda domain": {
        a = a as TermLambda;
        return collectContext(a.domain, ix, gamma);
      }
      case "lambda body": {
        a = a as TermLambda;
        return collectContext(a.body, ix, gamma.push([a.label, a.domain, undefined]));
      }
      case "let domain": {
        a = a as TermLet;
        return collectContext(a.domain, ix, gamma);
      }
      case "let argument": {
        a = a as TermLet;
        return collectContext(a.argument, ix, gamma);
      }
      case "let body": {
        a = a as TermLet;
        return collectContext(a.body, ix, gamma.push([a.label, a.domain, a.argument]));
      }
      case "application argument": {
        a = a as TermNeutral;
        return collectContext(a.arguments.get(step.iArg) as Term, ix, gamma);
      }
      default: throw Error();
    }
  } else {
    return [gamma, a];
  }
}

// infers the list of paramter types for a type
export function inferParameters(alpha: Term): List<Term> {
  switch (alpha.case) {
    case "pi": return inferParameters(alpha.codomain).insert(0, alpha.domain);
    default: return List();
  }
}