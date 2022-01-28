import React from 'react';
import './App.css';
import { AppState } from './AppState';
import { Environment } from './shape/Environment';

export type AppProps = {

}

export default class App extends React.Component<AppProps, AppState> {
  appEnvironment: AppState;

  constructor(props: Props) {
    super(props);
    this.appEnvironment = new AppState();
  }

  render(): JSX.Element {
    return (<div></div>); // TODO
  }
}

export type Props = {

}