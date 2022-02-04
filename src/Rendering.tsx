import { List } from "immutable";
import React, { MouseEventHandler } from "react";
import { AppState } from "./AppState";
import { Environment } from "./shape/Environment";
import { Label, Term, Hole, TermIx } from "./shape/Grammar";
import { applyTransformation, placePi } from "./shape/Transformation";

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
      {renderProgram(appState, appState.environment.get("program"))}
    </div>
  );
}

function renderPalette(appState: AppState): JSX.Element {
  let options: List<{
    label: string;
    onClick: MouseEventHandler<HTMLDivElement>;
  }> = List([
    {
      label: "pi",
      onClick: (event) => {
        console.log("before");
        console.log("focus", appState.environment.focus);
        console.log("program", appState.environment.program);
        let env: Environment = applyTransformation(
          appState.environment,
          placePi
        ) as Environment;
        console.log("after");
        console.log("focus", env.focus);
        console.log("program", env.program);
        if (env !== undefined) appState.environment = env;
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

function renderProgram(appState: AppState, program: Term): JSX.Element {
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
        console.log(a.format);
        if (a.arguments.size === 0) {
          return (a.format?.indented ? [br, indent(i)] : []).concat(
            typeof a.applicant === "number"
              ? [renderLabel(labels.get(a.applicant) as Label)]
              : [renderHole(a.applicant)]
          );
        } else {
          return (a.format?.indented ? [br, indent(i)] : [])
            .concat(lparen)
            .concat(
              typeof a.applicant === "number"
                ? [renderLabel(labels.get(a.applicant) as Label)]
                : [renderHole(a.applicant)]
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

  function renderHole(hole: Hole): JSX.Element {
    return <span className="hole">{question}</span>;
  }

  function renderLabel(label: Label): JSX.Element {
    //TODO: allow editting
    return <span className="token label">{label.value}</span>;
  }

  return <span>{renderTerm(program)}</span>;
}
