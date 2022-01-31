import { List, Record } from "immutable";
import { freshHole, HoleId, makeHole, Term } from "./Grammar";

/*
The environment contains all the global, mutable information necessary for typechecking and transformation.
*/

type EnvironmentProps = {
  program: Term
}

export const defaultEnvironmentProps: EnvironmentProps = {
  program: freshHole()
}

export class Environment extends Record(defaultEnvironmentProps) {}
