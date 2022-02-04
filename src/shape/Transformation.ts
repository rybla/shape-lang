// A transformation is a function that inputs a Term and outputs a Term.

import { Context, DeBruijnLevel, freshHoleTerm, Label, Term, TermIx, TermIxStep } from "./Grammar";
import { Environment } from "./Environment";
import { List } from "immutable";
import { infer } from "./Typing";

type Transformation = (env: Environment, gamma: Context, alpha: Term, a: Term) => Term | undefined

export function applyTransformation(env: Environment, trans: Transformation): Environment | undefined {
  function go(ix: TermIx, gamma: Context, a: Term): Environment | undefined {
    if (ix.size === 0 ) {
      let b = trans(env, gamma, infer(gamma, a), a);
      if (b) {
        return env.set("program", substituteTermIx(env.program, ix, b));
      } else {
        return undefined;
      }
    } else {
      let step = ix.last() as TermIxStep;
      switch (a.case) {
        case "pi": {
          switch (step.case) {
            case "pi domain": {
              return go(ix.pop(), gamma, a.domain);
            }
            case "pi codomain": {
              return go(ix.pop(), gamma.push([a.label, a.domain, undefined]), a.domain);
            }
          }
          throw new Error("impossible case");
        }
        case "lambda": {
          switch (step.case) {
            case "lambda domain": {
              return go(ix.pop(), gamma, a.domain);
            }
            case "lambda body": {
              return go(ix.pop(), gamma.push([a.label, a.domain, undefined]), a.body);
            }
          }
          throw new Error("impossible case");
        }
        case "let": {
          switch (step.case) {
            case "let domain": {
              return go(ix.pop(), gamma, a.domain);
            }
            case "let argument": {
              return go(ix.pop(), gamma, a.argument);
            }
            case "let body": {
              return go(ix.pop(), gamma.push([a.label, a.domain, a.argument]), a.body);
            }
          }
          throw new Error("impossible case");
        }
        case "neutral": {

        }
        // case "application": {
        //   switch (step.case) {
        //     case "application applicant": {
        //       return go(ix.pop(), gamma, a.applicant);
        //     }
        //     case "application argument": {
        //       return go(ix.pop(), gamma, a.argument);
        //     }
        //   }
        // }
      }
      throw new Error("impossible case");
    }
  }
  return go(env.focus, List(), env.program);
}



// transformations

export function freshLabel(): Label {
  return ({value: "x"});
}

export const placePi: Transformation = (env, gamma, alpha, a) => {
  return {case: "pi", label: freshLabel(), domain: freshHoleTerm(), codomain: freshHoleTerm()};
}

export function placeVar(dbl: DeBruijnLevel): Transformation {
  return (env, gamma, alpha, a) => {
    if (0 < dbl && dbl < gamma.size) {
      return {case: "neutral", applicant: dbl, arguments: List()};
    } else {
      return undefined;
    }
  }
}

// i.e. dig
export const placeHole: Transformation = (env, gamma, alpha, a) => {
  return freshHoleTerm();
}

// substitution

// a[ix => b]
export function substituteTermIx(a: Term, ix: TermIx, b: Term): Term {
  if (ix.size === 0) {
    return b;
  } else {
    let step = ix.last() as TermIxStep;
    switch (a.case) {
      case "pi": {
        switch (step.case) {
          case "pi domain": {
            return substituteTermIx(a.domain, ix.pop(), b);
          }
          case "pi codomain": {
            return substituteTermIx(a.codomain, ix.pop(), b);
          }
        }
        throw new Error("impossible case");
      }
      case "lambda": {
        switch (step.case) {
          case "lambda domain": {
            return substituteTermIx(a.domain, ix.pop(), b);
          }
          case "lambda body": {
            return substituteTermIx(a.body, ix.pop(), b);
          }
        }
        throw new Error("impossible case");
      }
      case "let": {
        switch (step.case) {
          case "let domain": {
            return substituteTermIx(a.domain, ix.pop(), b);
          }
          case "let argument": {
            return substituteTermIx(a.argument, ix.pop(), b);
          }
          case "let body": {
            return substituteTermIx(a.body, ix.pop(), b);
          }
        }
        throw new Error("impossible case");
      }
      case "neutral": {
        switch (step.case) {
          case "application argument": {
            return substituteTermIx(a.arguments.get(step.iArg) as Term, ix.pop(), b);
          }
        }
      }
    }
  }
  throw new Error("impossible case");
}