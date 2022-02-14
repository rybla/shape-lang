import { DeBruijn } from "./Syntax";

export type Transition =
  | {
      case: "block",
      sub:
        | {case: "insert"}
        | {case: "delete"}
    }
  | {
      case: "term",
      sub:
        | {case: "fill universe"}
        | {case: "fill pi"}
        | {case: "fill lambda"}
        | {case: "fill neutral", applicant: DeBruijn}
        | {case: "dig"}
    }
  | {
      case: "parameter",
      sub:
        | {case: "insert"}
        | {case: "delete"}
    }
  | {
      case: "label",
      sub:
        | {case: "append", value: string}
        | {case: "backspace"}
    }
  | {
      case: "format",
      sub:
        | {case: "toggle indented"}
        | {case: "toggle unannotated"}
    }
  