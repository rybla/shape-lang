// A transformation is a function that inputs a Term and outputs a Term.

import { Context, Dbl, Label, Term, TermIx } from "./Grammar";
import { Environment } from "./Environment";

type Transformation<Arg> = (env: Environment, ix: TermIx, gamma: Context, alpha: Term, a: Term, arg: Arg) => [Environment, Term] | undefined

export function applyTransformation<Arg>(env: Environment, ix: TermIx, trans: Transformation<Arg>, arg: Arg): Environment | undefined {
  throw new Error("unimplemented");
}

// transformations

export const placePi: Transformation<[]> = (env, ix, gamma, alpha, a, _) => {
  let label: Label = "x";
  let dom: Term = {case: "hole", hole: {holeId: env.get("freshHoleId")}};
  let cod: Term = {case: "hole", hole: {holeId: env.get("freshHoleId") + 1}};
  return [
    env.set("freshHoleId", env.get("freshHoleId") + 2),
    {case: "pi", label, dom, cod}
  ];
}

// TODO: placeLam, placeApp, placeHole

export const placeVar: Transformation<Dbl> = (env, ix, gamma, alpha, a, dbl: number) => {
  if (dbl < gamma.size) {
    return [
      env,
      {case: "var", dbl}
    ];
  } else {
    return undefined;
  }
}