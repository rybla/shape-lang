import React from 'react';
import './App.css';
import { AppState } from './AppState';
import { renderApp } from './Rendering';
import { Environment } from './shape/Environment';

export type AppProps = {

}

export default class App extends React.Component<AppProps, AppState> {
  appState: AppState;

  constructor(props: AppProps) {
    super(props);
    this.appState = new AppState();
  }

  render(): JSX.Element {
    return renderApp(this.appState)
  }
}
