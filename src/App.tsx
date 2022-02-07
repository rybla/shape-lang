import React from "react";
import "./App.css";
import { AppState } from "./AppState";
import { renderApp } from "./Rendering";

export type AppProps = {};

export var app: App;

export default class App extends React.Component<AppProps, AppState> {
  appState: AppState;

  constructor(props: AppProps) {
    super(props);
    this.appState = new AppState();
    app = this;
  }

  render(): JSX.Element {
    return renderApp();
  }
}
