import { Environment } from "./shape/Environment";

export class AppState {
  environment: Environment;

  constructor() {
    this.environment = new Environment();
  }
}