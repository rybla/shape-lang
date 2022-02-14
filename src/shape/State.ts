import { List, Record } from "immutable";
import { Block, defaultFormat, freshHole, makeBlock } from "./Syntax";

export type StateProps = {
  block: Block,
}

export const defaultState: StateProps = {
  block: makeBlock({
    bindings: List(),
    body: freshHole(),
    format: defaultFormat()
  })
}

export class State extends Record(defaultState) {}