// A transformation is a function that inputs a Term and outputs a Term.

import { Context, DeBruijnLevel, freshHole, Label, Term, TermIx } from "./Grammar";
import { Environment } from "./Environment";

type Transformation = (env: Environment, ix: TermIx, gamma: Context, alpha: Term, a: Term) => Term | undefined

export function applyTransformation(env: Environment, ix: TermIx, trans: Transformation): Environment | undefined {
  throw new Error("unimplemented");
}

// transformations

export function freshLabel(): Label {
  return ({value: "x"});
}

export const placePi: Transformation = (env, ix, gamma, alpha, a) => {
  return {case: "pi", label: freshLabel(), domain: freshHole(), codomain: freshHole()};
}

export function placeVar(debruijnlevel: DeBruijnLevel): Transformation {
  return (env, ix, gamma, alpha, a) => {
    if (0 < debruijnlevel && debruijnlevel < gamma.size) {
      return {case: "variable", debruijnlevel};
    } else {
      return undefined;
    }
  }
}

// i.e. dig
export const placeHole: Transformation = (env, ix, gamma, alpha, a) => {
  return freshHole();
}
