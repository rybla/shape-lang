// A transformation is a function that inputs a Term and outputs a Term.

import { Context, DeBruijnLevel, freshHoleTerm, Label, showTerm, Term, TermIx, TermIxStep } from "./Grammar";
import { Environment } from "./Environment";
import { List } from "immutable";
import { infer } from "./Typing";

type Transformation = (env: Environment, gamma: Context, alpha: Term, a: Term) => Term | undefined

export function applyTransformation(env: Environment, trans: Transformation): Environment | undefined {
  console.log("applyTransformation", showTerm(env.program), env.focus.toArray());
  function go(ix: TermIx, gamma: Context, a: Term): Term | undefined {
    console.log("applyTransformation.go", ix.toArray(), gamma.map(item => item[0].value).toArray(), showTerm(a));
    if (ix.size === 0 ) {
      return trans(env, gamma, infer(gamma, a), a);
    } else {
      let step = ix.first() as TermIxStep;
      ix = ix.shift();
      switch (a.case) {
        case "pi": {
          switch (step.case) {
            case "pi domain": {
              let domain = go(ix, gamma, a.domain);
              if (domain) return {case: "pi", label: a.label, domain, codomain: a.codomain }
              else return undefined;
            }
            case "pi codomain": {
              let codomain = go(ix, gamma.push([a.label, a.domain, undefined]), a.codomain);
              if (codomain) return {case: "pi", label: a.label, domain: a.domain, codomain};
              else return undefined;
            }
            default: return undefined;
          }
        }
        case "lambda": {
          switch (step.case) {
            case "lambda domain": {
              let domain = go(ix, gamma, a.domain);
              if (domain) return {case: "lambda", label: a.label, domain, body: a.body};
              else return undefined;
            }
            case "lambda body": {
              let body = go(ix, gamma.push([a.label, a.domain, undefined]), a.body);
              if (body) return {case: "lambda", label: a.label, domain: a.domain, body};
              else return undefined;
            }
            default: return undefined;
          }
        }
        case "let": {
          switch (step.case) {
            case "let domain": {
              let domain = go(ix, gamma, a.domain);
              if (domain) return {case: "let", label: a.label, domain, argument: a.argument, body: a.body};
              return undefined;
            }
            case "let argument": {
              let argument = go(ix, gamma, a.argument);
              if (argument) return {case: "let", label: a.label, domain: a.domain, argument, body: a.body};
              else return undefined;
            }
            case "let body": {
              let body = go(ix, gamma.push([a.label, a.domain, a.argument]), a.body);
              if (body) return {case: "let", label: a.label, domain: a.domain, argument: a.argument, body};
              else return undefined;
            }
            default: return undefined;
          }
        }
        case "neutral": {
          switch (step.case) {
            case "application argument": {
              let argument = go(ix, gamma, a.arguments.get(step.iArg) as Term);
              if (argument) return {case: "neutral", applicant: a.applicant, arguments: a.arguments.set(step.iArg, argument)};
              else return undefined;
            }
          }
        }
      }
    }
  }
  let programNew = go(env.focus, List(), env.program);
  if (programNew) return env.set("program", programNew);
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

// // a[ix => b]
// export function substituteTermIx(a: Term, ix: TermIx, b: Term): Term {
//   if (ix.size === 0) {
//     return b;
//   } else {
//     let step = ix.last() as TermIxStep;
//     switch (a.case) {
//       case "pi": {
//         switch (step.case) {
//           case "pi domain": {
//             return substituteTermIx(a.domain, ix, b);
//           }
//           case "pi codomain": {
//             return substituteTermIx(a.codomain, ix.pop(), b);
//           }
//         }
//         throw new Error("impossible case");
//       }
//       case "lambda": {
//         switch (step.case) {
//           case "lambda domain": {
//             return substituteTermIx(a.domain, ix.pop(), b);
//           }
//           case "lambda body": {
//             return substituteTermIx(a.body, ix.pop(), b);
//           }
//         }
//         throw new Error("impossible case");
//       }
//       case "let": {
//         switch (step.case) {
//           case "let domain": {
//             return substituteTermIx(a.domain, ix.pop(), b);
//           }
//           case "let argument": {
//             return substituteTermIx(a.argument, ix.pop(), b);
//           }
//           case "let body": {
//             return substituteTermIx(a.body, ix.pop(), b);
//           }
//         }
//         throw new Error("impossible case");
//       }
//       case "neutral": {
//         switch (step.case) {
//           case "application argument": {
//             return substituteTermIx(a.arguments.get(step.iArg) as Term, ix.pop(), b);
//           }
//         }
//       }
//     }
//   }
//   throw new Error("impossible case");
// }