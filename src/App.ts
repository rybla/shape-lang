import { List, Record } from "immutable";
import React from "react";
import "./App.css";
import { AppEventHandler } from "./AppEventHandler";
import { AppRenderer } from "./AppRenderer";
import { AppUpdater } from "./AppUpdater";
import { State } from "./shape/State";
import { Index, Label } from "./shape/Syntax";

export default class App extends React.Component<AppProps, AppState> {
  appState: AppState
  appEventHandler: AppEventHandler
  appUpdater: AppUpdater
  appRenderer: AppRenderer

  constructor(props: AppProps) {
    super(props)
    this.appState = new AppState()
    this.appEventHandler = new AppEventHandler(this)
    this.appUpdater = new AppUpdater(this)
    this.appRenderer = new AppRenderer(this)
  }

  render(): JSX.Element {
    return this.appRenderer.render()
  }
}

// AppProps

export type AppProps = {}

// AppState

export type AppStateProps = {
  state: State,
  mode: Mode
}

export type Mode =
  | {
      case: "normal",
      focus: Index
    }
  | {
      case: "label",
      focus: Index,
      label: Label
    }

export const defaultAppState: AppStateProps = {
  state: new State(),
  mode: {case: "normal", focus: List()}
}

export class AppState extends Record(defaultAppState) {}