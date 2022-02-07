import { List } from "immutable";
import { Environment } from "./shape/Environment";
import { Label, TermIx } from "./shape/Grammar";

type Mode
  = {case: "label", focus: TermIx, label: Label}
  | {case: "term", focus: TermIx}

export class AppState {
  environment: Environment;
  mode: Mode;

  constructor() {
    this.environment = new Environment();
    this.mode = {case: "term", focus: List()};
  }
}