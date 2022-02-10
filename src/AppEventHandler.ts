import App from "./App";

const Keys_ArrowDirections: string[] = [
  "ArrowLeft",
  "ArrowRight",
  "ArrowUp",
  "ArrowDown",
]

const Keys_Ignore_LabelMode: string[] = [
  "Meta",
  "Shift",
  "Alt"
]

export class AppEventHandler {
  app: App

  constructor(app: App) {
    this.app = app;
    window.addEventListener("keydown", this.handleKeyDown)
  }

  handleKeyDown(event: KeyboardEvent): void {
    switch (this.app.appState.mode.case) {
      case "term": {
        throw new Error()
      }
      case "label": {
        if (Keys_ArrowDirections.includes(event.key))  {
          
        } else if (Keys_Ignore_LabelMode.includes(event.key)) {
          return;
        } else if (event.key === "Tab") {

        } else if (event.key === "Enter") {

        }
      }
    }
  }
}