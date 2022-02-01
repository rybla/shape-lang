import { List, Record } from "immutable";
import { freshHole, HoleId, makeHole, Term } from "./Grammar";

/*
The environment contains all the global, mutable information necessary for typechecking and transformation.
*/

type EnvironmentProps = {
  program: Term
}

export const defaultEnvironmentProps: EnvironmentProps = {
  program: {
    case: "let",
    label: {value: "x"},
    domain: {
      case: "let",
      label: {value: "x"},
      domain: freshHole(),
      argument: freshHole(),
      body: freshHole(),
    },
    argument: {
      case: "let",
      label: {value: "x"},
      domain: freshHole(),
      argument: freshHole(),
      body: {
        case: "pi",
        label: {value: "y"},
        domain: freshHole(),
        codomain: freshHole(),
      }
    },
    body: {
      case: "let",
      label: {value: "x"},
      domain: freshHole(),
      argument: freshHole(),
      body: {
        case: "let",
        label: {value: "x"},
        domain: freshHole(),
        argument: freshHole(),
        body: freshHole()
      }
    }
  }
}

export class Environment extends Record(defaultEnvironmentProps) {}
