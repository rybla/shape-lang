import App from "./App";

export class AppRenderer {
  app: App;

  constructor(app: App) {
    this.app = app;
  }

  render(): JSX.Element {
    throw new Error();
  }
}
