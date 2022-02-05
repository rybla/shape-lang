import { List, Record } from "immutable";
import { freshHoleTerm, Term, TermIx } from "./Grammar";

/*
The environment contains all the global, mutable information necessary for typechecking and transformation.
*/

type EnvironmentProps = {
  program: Term,
  focus: TermIx
}

export const defaultEnvironmentProps: EnvironmentProps = {
  focus: List(),
  program: freshHoleTerm()
  // program: {
  //   case: "let",
  //   label: {value: "x"},
  //   domain: freshHoleTerm(),
  //   argument: freshHoleTerm(),
  //   body: freshHoleTerm()
  // }
  // program: {
  //   case: "let",
  //   label: {value: "x"},
  //   domain: {
  //     case: "let",
  //     label: {value: "x"},
  //     domain: freshHoleTerm(),
  //     argument: freshHoleTerm(),
  //     body: freshHoleTerm(),
  //   },
  //   argument: {
  //     case: "let",
  //     label: {value: "x"},
  //     domain: freshHoleTerm(),
  //     argument: freshHoleTerm(),
  //     body: {
  //       case: "pi",
  //       label: {value: "y"},
  //       domain: freshHoleTerm(),
  //       codomain: freshHoleTerm(),
  //     }
  //   },
  //   body: {
  //     case: "let",
  //     label: {value: "x"},
  //     domain: freshHoleTerm(),
  //     argument: freshHoleTerm(),
  //     body: {
  //       case: "let",
  //       label: {value: "x"},
  //       domain: freshHoleTerm(),
  //       argument: freshHoleTerm(),
  //       body: freshHoleTerm()
  //     }
  //   }
  // }
}

export class Environment extends Record(defaultEnvironmentProps) {}
