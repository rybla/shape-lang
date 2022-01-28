import { Record } from "immutable";
import { HoleId, Term } from "./Grammar";

/*
The environment contains all the global, mutable information necessary for typechecking and transformation.
*/

type EnvironmentProps = {
  freshHoleId: HoleId,
  program: Term
}

export const defaultEnvironmentProps: EnvironmentProps = {
  freshHoleId: 0,
  program: {case: "hole", hole: {holeId: 0}}
}

export class Environment extends Record(defaultEnvironmentProps) {}
