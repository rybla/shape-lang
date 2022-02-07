import { List, Record } from "immutable";
import { freshHoleTerm, Label, Term, TermIx } from "./Grammar";

/*
The environment contains all the global, mutable information necessary for typechecking and transformation.
*/



type EnvironmentProps = {
  program: Term,
}

export const defaultEnvironmentProps: EnvironmentProps = {
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
