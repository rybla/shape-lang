import App from "../App";
import { State } from "./State";
import { Transition } from "./Transition";

export class Updater {
  app: App

  constructor(app: App) {
    this.app = app;
  }

  update(state: State, transition: Transition): State | undefined {
    throw new Error()
  }
}