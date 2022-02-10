import { List } from "immutable";
import App, { AppState } from "./App";
import { AppTransition } from "./AppTransition";
import { State } from "./shape/State";
import { Updater } from "./shape/Updater";

export class AppUpdater {
  app: App
  updater: Updater

  constructor(app: App) {
    this.app = app
    this.updater = new Updater(app)
  }

  apply(appTransitions: List<AppTransition>): boolean {
    let appState = appTransitions.reduce(
      (appState, appTransition) =>
        appState ?
          this.update(appState, appTransition) :
          undefined,
      this.app.appState as AppState | undefined
    )
    if (appState) {
      this.app.setState(appState)
      return true
    } else {
      return false
    }
  }

  update(appState: AppState, appTransition: AppTransition): AppState | undefined {
    switch (appTransition.case) {
      case "transitions": {
        let state: State | undefined = appTransition.transitions.reduce(
          (state, transitition) => 
            state ?
              this.updater.update(state, transitition) :
              undefined,
            appState.state as State | undefined
        )
        if (state)
          return appState.set("state", state);
        else 
          return undefined
      }
      case "navigation": {
        throw new Error()
      }
      case "mode": {
        throw new Error()
      }
    }
  }
}