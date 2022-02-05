import { List } from "immutable";
import React, { MouseEventHandler } from "react";
import App from "./App";
import { Environment } from "./shape/Environment";
import { Label, Term, Hole, TermIx } from "./shape/Grammar";
import { applyTransformation, placePi } from "./shape/Transformation";

export function renderApp(app: App): JSX.Element {
  return (
    <div className="App">
      {renderWorkspace(app)}
      <div className="sidebar">
        {renderPalette(app)}
        {renderConsole(app)}
        {renderCommandLine(app)}
      </div>
    </div>
  );
}

function renderWorkspace(app: App): JSX.Element {
  return (
    <div className="workspace">
      {renderProgram(app, app.appState.environment.program)}
    </div>
  );
}

function renderPalette(app: App): JSX.Element {
  let options: List<{
    label: string;
    onClick: MouseEventHandler<HTMLDivElement>;
  }> = List([
    {
      label: "pi",
      onClick: (event) => {
        let envNew = applyTransformation(
          app.appState.environment,
          placePi
        ) as Environment;
        if (envNew !== undefined) {
          console.log("transformation success");
          app.appState.environment = envNew;
          app.setState(app.appState);
        } else {
          console.log("transformation failure");
        }
      },
    },
  ]);
  return (
    <div className="palette">
      {options.map((opt) => (
        <div className="palette-option" onClick={opt.onClick}>
          {opt.label}
        </div>
      ))}
    </div>
  );
}

function renderConsole(app: App): JSX.Element {
  return (
    <div className="console">
      {}
      {}
    </div>
  );
}

function renderCommandLine(app: App): JSX.Element {
  return (
    <div className="commandline">
      {}
      {}
    </div>
  );
}

function renderProgram(app: App, program: Term): JSX.Element {
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
    i: number = 0,
    ix: TermIx = List()
  ): JSX.Element[] {
    if (a.case !== "neutral") {
      a.format = {
        indented: true,
        annotated: true,
      };
    }
    switch (a.case) {
      case "universe": {
        return [<span className="term universe">U</span>];
      }
      case "pi": {
        let iSub = a.format?.indented ? i + 1 : i;
        return (a.format?.indented ? [br, indent(i)] : [])
          .concat([lparen, pi, renderLabel(a.label)])
          .concat(
            a.format?.annotated
              ? [colon].concat(
                  renderTerm(
                    a.domain,
                    labels,
                    iSub,
                    ix.push({ case: "pi domain" })
                  )
                )
              : []
          )
          .concat(period)
          .concat(
            renderTerm(
              a.codomain,
              labels.insert(0, a.label),
              iSub,
              ix.push({ case: "pi codomain" })
            )
          )
          .concat(rparen);
      }
      case "lambda": {
        let iSub = a.format?.indented ? i + 1 : i;
        return (a.format?.indented ? [br, indent(i)] : [])
          .concat([lparen, lambda, renderLabel(a.label)])
          .concat(
            a.format?.annotated
              ? [colon].concat(
                  renderTerm(
                    a.domain,
                    labels,
                    iSub,
                    ix.push({ case: "lambda domain" })
                  )
                )
              : []
          )
          .concat([period])
          .concat(
            renderTerm(
              a.body,
              labels.insert(0, a.label),
              iSub,
              ix.push({ case: "lambda body" })
            )
          )
          .concat([rparen]);
      }
      case "let": {
        let iSub = a.format?.indented ? i + 1 : i;
        return (a.format?.indented ? [br, indent(i)] : [])
          .concat([lparen, let_, renderLabel(a.label)])
          .concat(
            a.format?.annotated
              ? [colon].concat(
                  renderTerm(
                    a.domain,
                    labels,
                    iSub,
                    ix.push({ case: "let domain" })
                  )
                )
              : []
          )
          .concat(assignment)
          .concat(
            renderTerm(
              a.argument,
              labels,
              iSub,
              ix.push({ case: "let argument" })
            )
          )
          .concat(in_)
          .concat(
            renderTerm(
              a.body,
              labels.insert(0, a.label),
              iSub,
              ix.push({ case: "let body" })
            )
          )
          .concat(rparen);
      }
      case "neutral": {
        let iSub = a.format?.indented ? i + 1 : i;
        if (a.arguments.size === 0) {
          return (a.format?.indented ? [br, indent(i)] : []).concat(
            typeof a.applicant === "number"
              ? [renderLabel(labels.get(a.applicant) as Label)]
              : [renderHole(a.applicant, ix)]
          );
        } else {
          return (a.format?.indented ? [br, indent(i)] : [])
            .concat(lparen)
            .concat(
              typeof a.applicant === "number"
                ? [renderLabel(labels.get(a.applicant) as Label)]
                : [renderHole(a.applicant, ix)]
            )
            .concat(
              a.arguments
                .map((a, iArg) =>
                  renderTerm(
                    a,
                    labels,
                    iSub,
                    ix.push({ case: "application argument", iArg })
                  )
                )
                .toArray()
                .reduce((a, b) => a.concat([space], b))
            )
            .concat(rparen);
        }
      }
    }
  }

  function renderHole(hole: Hole, ix: TermIx): JSX.Element {
    let onClick: MouseEventHandler<HTMLSpanElement> = (event) => {
      app.appState.environment = app.appState.environment.set("focus", ix);
      app.setState(app.appState);
    };
    return (
      <span className="hole" onClick={onClick}>
        {question}
      </span>
    );
  }

  function renderLabel(label: Label): JSX.Element {
    //TODO: allow editting
    return <span className="token label">{label.value}</span>;
  }

  return <span>{renderTerm(program)}</span>;
}
