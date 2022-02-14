import App from "../App";
import { State } from "./State";
import { Transition } from "./Transition";
import { Block, BlockProps } from "./Syntax"
import { Record } from "immutable";

export class Updater {
  app: App

  constructor(app: App) {
    this.app = app
  }

  update(state: State, transition: Transition): State | undefined {
    switch (transition.case) {
      case "block": {
        switch (transition.sub.case) {
          case "insert": {
            if (this.app.appState.mode.case === "normal") {
              // TODO: how to deeply update the records?
              state.update("block", (block) => block.updateIn(this.app.appState.mode.focus, (target) => {
                if (Record.isRecord<BlockProps>)
                // if (Record.isRecord(target)) {
                //   // target.set("")
                // }
              }))
              throw new Error()
            }
            throw new Error()
          }
          case "delete": throw new Error()
        }
        throw new Error()
      }
      case "term": {
        switch (transition.sub.case) {
          case "fill universe": throw new Error()
          case "fill pi": throw new Error()
          case "fill lambda": throw new Error()
          case "fill neutral": throw new Error()
          case "dig": throw new Error()
        }
        throw new Error()
      }
      case "parameter": {
        switch (transition.sub.case) {
          case "insert": throw new Error()
          case "delete": throw new Error()
        }
        throw new Error()
      }
      case "format": {
        switch (transition.sub.case) {
          case "toggle indented": throw new Error()
          case "toggle unannotated": throw new Error()
        }
        throw new Error()
      }
    }
  }
}