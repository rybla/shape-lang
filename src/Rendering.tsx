import { List } from "immutable";
import { AppState } from "./AppState";
import { Dbl, Label, Term } from "./shape/Grammar";

export function renderApp(appState: AppState): JSX.Element {
  return (
    <div className="App">
      {renderWorkspace(appState)}
      <div className="sidebar">
        {renderPalette(appState)}
        {renderConsole(appState)}
        {renderCommandLine(appState)}
      </div>
    </div>
  );
}

function renderWorkspace(appState: AppState): JSX.Element {
  return (
    <div className="workspace">
      {renderProgram(appState.environment.get("program"))}
    </div>
  );
}

function renderPalette(appState: AppState): JSX.Element {
  return (
    <div className="palette">
      {}
      {}
    </div>
  );
}

function renderConsole(appState: AppState): JSX.Element {
  return (
    <div className="console">
      {}
      {}
    </div>
  );
}

function renderCommandLine(appState: AppState): JSX.Element {
  return (
    <div className="commandline">
      {}
      {}
    </div>
  );
}

function renderProgram(program: Term): JSX.Element {
  return renderTerm(List(), program);
}

// punctuation
const per = <span className="punctuation period">.</span>;
const col = <span className="punctuation colon">:</span>;
const lpar = <span className="punctuation left-parenthesis">(</span>;
const rpar = <span className="punctuation right-parenthesis">)</span>;
const pi = <span className="punctuation pi">Π</span>;
const lam = <span className="punctuation lambda">λ</span>;
const let_ = <span className="punctuation let">let</span>;
const in_ = <span className="punctuation in">in</span>;
const ass = <span className="punctuation assignment">=</span>;
const que = <span className="punctuation question-mark">?</span>;

function renderTerm(labels: List<Label>, a: Term): JSX.Element {
  switch (a.case) {
    case "uni":
      return <span className="term uni">U</span>;
    case "pi":
      return (
        <span className="term pi">
          {lpar}
          {pi}
          {renderLabel(a.label)}
          {col}
          {renderTerm(labels, a.dom)}
          {per}
          {renderTerm(labels.insert(0, a.label), a.cod)}
          {rpar}
        </span>
      );
    case "lam":
      return (
        <span className="term lam">
          {lpar}
          {lam}
          {renderLabel(a.label)}
          {col}
          {renderTerm(labels, a.dom)}
          {per}
          {renderTerm(labels.insert(0, a.label), a.bod)}
          {rpar}
        </span>
      );
    case "let":
      return (
        <span className="term let">
          <span className="let-arg">
            {let_}
            {renderLabel(a.label)}
            {col}
            {renderTerm(labels, a.dom)}
            {ass}
            {renderTerm(labels, a.arg)}
            {in_}
          </span>
          <span className="let-bod">
            {renderTerm(labels.insert(0, a.label), a.bod)}
          </span>
        </span>
      );
    case "app":
      return (
        <span className="term app">
          {lpar}
          {renderTerm(labels, a.app)}
          {renderTerm(labels, a.arg)}
          {rpar}
        </span>
      );
    case "var":
      return <span className="term var">{labels.get(a.dbl)}</span>;
    case "hole":
      return <span className="term hole">{que}</span>;
  }
}

function renderLabel(label: Label): JSX.Element {
  //TODO: allow editting
  return <span className="label">{label.value}</span>;
}
