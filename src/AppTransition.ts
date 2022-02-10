import { Transition } from "./shape/Transition";

export type AppTransition =
  | {
      case: "transition"
      transition: Transition
    }
  | {
      case: "navigation"
      sub:
        | {case: "up"}
        | {case: "down"}
        | {case: "left"}
        | {case: "right"}
        | {case: "next"}
        | {case: "previous"}
        | {case: "top"}
    }
  | {
      case: "mode"
      sub:
        | {case: "term"}
        | {case: "label"}
    }
