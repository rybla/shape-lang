// A transformation is a function that inputs a Term and outputs a Term.

import { Context, DeBruijnLevel, Hole, HoleId, Label, makeHole, Term, TermHole, TermIx } from "./Grammar";
import { Environment } from "./Environment";
import { List } from "immutable";

type Transformation = (env: Environment, ix: TermIx, gamma: Context, alpha: Term, a: Term) => [Environment, Term] | undefined

export function applyTransformation(env: Environment, ix: TermIx, trans: Transformation): Environment | undefined {
  throw new Error("unimplemented");
}

// transformations

export function freshLabel(): Label {
  return ({value: "x"});
}

// mutable env
export function freshHole(env: Environment): Hole {
  let holeId: HoleId = env.get("freshHoleId");
  env.set("freshHoleId", holeId + 1);
  return {holeId};
}

// Creates an uninitialized hole
function newHole(): Hole {
  return {holeId: -1};
}

// Initializes a hole
// mutable env
function initHole(env: Environment, hole: Hole) {
  let holeId = env.get("freshHoleId");
  hole.holeId = holeId;
  env.set("freshHoleId", holeId + 1);
}

export const placePi: Transformation = (env, ix, gamma, alpha, a) => {
  let label: Label = freshLabel();
  let domain: TermHole = makeHole(newHole());
  let codomain: TermHole = makeHole(newHole());
  let envNew = env.withMutations(env => {
    initHole(env, domain.hole);
    initHole(env, codomain.hole);
  })
  return [envNew, {case: "pi", label, domain, codomain}];
}

export function placeVar(debruijnlevel: DeBruijnLevel): Transformation {
  return (env, ix, gamma, alpha, a) => {
    if (0 < debruijnlevel && debruijnlevel < gamma.size) {
      return [env, {case: "variable", debruijnlevel}];
    } else {
      return undefined;
    }
  }
}

// i.e. dig
export const placeHole: Transformation = (env, ix, gamma, alpha, a) => {
  let hole: TermHole = makeHole(newHole());
  let envNew = env.withMutations(env => {
    initHole(env, hole.hole);
  });
  return [envNew, hole]
}

// // i.e. "dig"
// export const placeHole: Transformation = (env1, ix, gamma, alpha, a) => {
//   let [env2, hole] = freshHole(env1);
//   return [env2, {case: "hole", hole, metadata: freshMetaData()}];
// }
