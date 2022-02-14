import App from "../App";
import { State } from "./State";
import { Transition } from "./Transition";

export class Updater {
  app: App

  constructor(app: App) {
    this.app = app
  }

  update(state: State, transition: Transition): State | undefined {
    switch (transition.case) {
      case "block": {
        switch (transition.sub.case) {
          case "insert": throw new Error()
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
      case "label": {
        switch (transition.sub.case) {
          case "append": throw new Error()
          case "backspace": throw new Error()
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