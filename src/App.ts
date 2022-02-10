import { List, Record } from "immutable";
import React from "react";
import "./App.css";
import { AppRenderer } from "./AppRenderer";
import { State } from "./shape/State";
import { Index } from "./shape/Syntax";

export default class App extends React.Component<AppProps, AppState> {
  appState: AppState
  appRenderer: AppRenderer

  constructor(props: AppProps) {
    super(props)
    this.appState = new AppState()
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
  focus: Index
  state: State
}

export const defaultAppState: AppStateProps = {
  focus: List(),
  state: new State()
}

export class AppState extends Record(defaultAppState) {}