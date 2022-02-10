import { DeBruijn } from "./Syntax"

export type Transition =
  | {
      case: "term"
      sub:
        | {case: "fill universe"}
        | {case: "fill pi"}
        | {case: "fill lambda"}
        | {case: "fill neutral", applicant: DeBruijn}
        | {case: "dig"}
    }
  | {
      case: "block"
      sub:
        | {case: "insert binding"}
    }
  | {
      case: "parameter"
      sub:
        | {case: "insert"}
    }
  | {
      case: "label"
      sub:
        | {case: "append", value: string}
        | {case: "backspace"}
    }
  