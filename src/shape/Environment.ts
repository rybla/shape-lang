import { Record } from "immutable";
import { Term } from "./Grammar";

/*
The environment contains all the global, mutable information necessary for typechecking and transformation.
*/

type EnvironmentProps = {
  program: Term
}

export const defaultEnvironmentProps: EnvironmentProps = {
  // program: {case: "hole"}
  program: {
    case: "let",
    label: {value: "x"},
    dom: {case: "hole"},
    arg: {
      case: "let",
      label: {value: "y"},
      dom: {case: "hole"},
      arg: {
        case: "let",
        label: {value: "z"},
        dom: {case: "hole"},
        arg: {case: "hole"},
        bod: {case: "hole"}
      },
      bod: {case: "hole"}
    },
    bod: {
      case: "let",
      label: {value: "w"},
      dom: {case: "hole"},
      arg: {case: "hole"},
      bod: {
        case: "let",
        label: {value: "w"},
        dom: {case: "hole"},
        arg: {case: "hole"},
        bod: {
          case: "let",
          label: {value: "w"},
          dom: {case: "hole"},
          arg: {case: "hole"},
          bod: {
            case: "let",
            label: {value: "w"},
            dom: {case: "hole"},
            arg: {case: "hole"},
            bod: {
              case: "let",
              label: {value: "w"},
              dom: {case: "hole"},
              arg: {case: "hole"},
              bod: {case: "hole"}
            }
          }
        }
      }
    }
  }
}

export class Environment extends Record(defaultEnvironmentProps) {}
