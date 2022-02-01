import { List } from "immutable";
import { AppState } from "./AppState";
import { Label, Term } from "./shape/Grammar";

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
  return <span>{renderTerm(program)}</span>;
}

// punctuation
const br = <br></br>;
const period = <span className="token punctuation period">.</span>;
const colon = <span className="token punctuation colon">:</span>;
const lparen = <span className="token punctuation left-paren">(</span>;
const rparen = <span className="token punctuation right-paren">)</span>;
const pi = <span className="token punctuation pi">Π</span>;
const lambda = <span className="token punctuation lambda">λ</span>;
const let_ = <span className="token punctuation let">let</span>;
const in_ = <span className="token punctuation in">in</span>;
const assignment = <span className="token punctuation assignment">=</span>;
const question = <span className="token punctuation question-mark">?</span>;
const space = <span className="token punctuation space"></span>;

function indent(i: number): JSX.Element {
  return (
    <span
      className="i"
      style={{ display: "inline-block", width: i + "em" }}
    ></span>
  );
}

function renderTerm(
  a: Term,
  labels: List<Label> = List(),
  i: number = 0
): JSX.Element[] {
  a.format = {
    indented: true,
    annotated: false,
  };
  switch (a.case) {
    case "universe": {
      return [<span className="term universe">U</span>];
    }
    case "pi": {
      let iSub = a.format?.indented ? i + 1 : i;
      return (a.format?.indented ? [br, indent(i)] : [])
        .concat([lparen, pi, renderLabel(a.label)])
        .concat(
          a.format.annotated
            ? [colon].concat(renderTerm(a.domain, labels, iSub))
            : []
        )
        .concat(period)
        .concat(renderTerm(a.codomain, labels.insert(0, a.label), iSub))
        .concat(rparen);
    }
    case "lambda": {
      let iSub = a.format?.indented ? i + 1 : i;
      return (a.format?.indented ? [br, indent(i)] : [])
        .concat([lparen, lambda, renderLabel(a.label)])
        .concat(
          a.format.annotated
            ? [colon].concat(renderTerm(a.domain, labels, iSub))
            : []
        )
        .concat(period)
        .concat(renderTerm(a.body, labels.insert(0, a.label), iSub))
        .concat(rparen);
    }
    case "let": {
      let iSub = a.format?.indented ? i + 1 : i;
      return (a.format?.indented ? [br, indent(i)] : [])
        .concat([lparen, let_, renderLabel(a.label)])
        .concat(
          a.format.annotated
            ? [colon].concat(renderTerm(a.domain, labels, iSub))
            : []
        )
        .concat(assignment)
        .concat(renderTerm(a.argument, labels, iSub))
        .concat(in_)
        .concat(renderTerm(a.body, labels.insert(0, a.label), iSub))
        .concat(rparen);
    }
    case "application": {
      let iSub = a.format?.indented ? i + 1 : i;
      return (a.format?.indented ? [br, indent(i)] : [])
        .concat(lparen)
        .concat(renderTerm(a.applicant, labels, iSub))
        .concat(space)
        .concat(renderTerm(a.argument, labels, iSub))
        .concat(rparen);
    }
    case "variable": {
      return (a.format?.indented ? [br, indent(i)] : []).concat(
        renderLabel(labels.get(a.debruijnlevel) as Label)
      );
    }
    case "hole": {
      return [<span className="hole">{question}</span>];
    }
  }
}

// function renderTerm(labels: List<Label>, a: Term): JSX.Element {
//   switch (a.case) {
//     case "universe":
//       return <span className="term uni">U</span>;
//     case "pi":
//       return (
//         <span className="term pi">
//           {lpar}
//           {pi}
//           {renderLabel(a.label)}
//           {col}
//           {renderTerm(labels, a.domain)}
//           {per}
//           {renderTerm(labels.insert(0, a.label), a.codomain)}
//           {rpar}
//         </span>
//       );
//     case "lambda":
//       return (
//         <span className="term lam">
//           {lpar}
//           {lam}
//           {renderLabel(a.label)}
//           {col}
//           {renderTerm(labels, a.domain)}
//           {per}
//           {renderTerm(labels.insert(0, a.label), a.body)}
//           {rpar}
//         </span>
//       );
//     case "let":
//       return (
//         <span className="term let">
//           <span className="let-let">{let_}</span>
//           <span className="let-arg">
//             {renderLabel(a.label)}
//             {col}
//             {renderTerm(labels, a.domain)}
//             {ass}
//             {renderTerm(labels, a.argument)}
//           </span>
//           <span className="let-in">{in_}</span>
//           <span className="let-bod">
//             {renderTerm(labels.insert(0, a.label), a.body)}
//           </span>
//         </span>
//       );
//     case "application":
//       return (
//         <span className="term app">
//           {lpar}
//           {renderTerm(labels, a.applicant)}
//           {renderTerm(labels, a.argument)}
//           {rpar}
//         </span>
//       );
//     case "variable":
//       return <span className="term var">{labels.get(a.debruijnlevel)}</span>;
//     case "hole":
//       return <span className="term hole">{que}</span>;
//   }
// }

function renderLabel(label: Label): JSX.Element {
  //TODO: allow editting
  return <span className="token label">{label.value}</span>;
}
