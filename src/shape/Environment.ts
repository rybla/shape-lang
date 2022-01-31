import { List, Record } from "immutable";
import { HoleId, makeHole, Term } from "./Grammar";

/*
The environment contains all the global, mutable information necessary for typechecking and transformation.
*/

type EnvironmentProps = {
  freshHoleId: HoleId,
  program: Term
}

export const defaultEnvironmentProps: EnvironmentProps = {
  freshHoleId: 1,
  program: makeHole({holeId: 0})
}

export class Environment extends Record(defaultEnvironmentProps) {}
