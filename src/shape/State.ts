import { List, Record } from "immutable";
import { Block, defaultFormat, freshHole } from "./Syntax";

export type StateProps = {
  block: Block,
}

export const defaultState: StateProps = {
  block: {
    bindings: List(),
    body: freshHole(),
    format: defaultFormat()
  }
}

export class State extends Record(defaultState) {}