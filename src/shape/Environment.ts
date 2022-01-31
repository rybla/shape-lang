import { List, Record } from "immutable";
import { freshHole, HoleId, makeHole, Term } from "./Grammar";

/*
The environment contains all the global, mutable information necessary for typechecking and transformation.
*/

type EnvironmentProps = {
  freshHoleId: HoleId,
  program: Term
}

export const defaultEnvironmentProps: EnvironmentProps = {
  freshHoleId: 1,
  program: freshHole()
}

export class Environment extends Record(defaultEnvironmentProps) {}
