import { Context, freshHoleTerm, Term, TermUniverse } from "./Grammar";

// Checks that `a` has type `alpha`.
export function check(gamma: Context, a: Term, alpha: Term): boolean {
  throw new Error("unimplemented");
}

// Infers the type of `a`.
export function infer(gamma: Context, a: Term): Term {
  switch (a.case) {
    case "universe": return {case: "universe", universelevel: a.universelevel + 1};
    case "pi": {
      let domainType = infer(gamma, a.domain) as TermUniverse;
      let codomainType = infer(gamma.push([a.label, a.domain, undefined]), a.codomain) as TermUniverse;
      let universelevel = Math.max(domainType.universelevel, codomainType.universelevel) + 1;
      return {case: "universe", universelevel};
    }
    case "lambda": {
      let codomain = infer(gamma.push([a.label, a.domain, undefined]), a.body);
      return {case:"pi", label: a.label, domain: a.domain, codomain};
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
