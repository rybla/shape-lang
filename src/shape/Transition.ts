import { Label } from "./Syntax";

export type Transition =
  | {
      case: "declaration",
      sub:
        | {case: "insert term"}
        | {case: "insert type"}
        | {case: "insert data"}
    }
  | {
      case: "kind",
      sub:
        | {case: "fill arrow"}
        | {case: "fill unit"}
        | {case: "dig"}
    }
  | {
      case: "type",
      sub:
        | {case: "fill arrow"}
        | {case: "fill neutral", applicant: Label}
        | {case: "dig"}
    }
  | {
      case: "block",
      sub:
        | {case: "insert"}
        | {case: "delete"}
    }
  | {
      case: "term",
      sub:
        | {case: "fill lambda"}
        | {case: "fill neutral", applicant: Label}
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
  