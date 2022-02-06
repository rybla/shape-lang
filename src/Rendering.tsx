import { List } from "immutable";
import React, { MouseEventHandler } from "react";
import App from "./App";
import { collectContext } from "./shape/Typing";
import {
  Label,
  Term,
  Context,
  Hole,
  TermIx,
  compareTermIx,
  showTerm,
} from "./shape/Grammar";
import {
  applyTransformation,
  placeLambda,
  placeLet,
  placeVariable,
  placePi,
  placeUniverse,
} from "./shape/Transformation";

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
      label: "universe",
      onClick: (event) => applyTransformation(app, placeUniverse),
    },
    {
      label: "pi",
      onClick: (event) => applyTransformation(app, placePi),
    },
    {
      label: "lambda",
      onClick: (event) => applyTransformation(app, placeLambda),
    },
    {
      label: "let",
      onClick: (event) => applyTransformation(app, placeLet),
    },
  ]);
  let env = app.appState.environment;
  let context: Context = collectContext(env.program, env.focus);
  context.forEach((item, dbl) => {
    options = options.push({
      label: "neutral " + item[0].value,
      onClick: (event) => {
        applyTransformation(app, placeVariable(dbl));
      },
    });
  });
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
    // if (a.case !== "neutral") {
    //   a.format = {
    //     indented: true,
    //     annotated: true,
    //   };
    // }
    switch (a.case) {
      case "universe": {
        return [
          <span className="term universe">
            U<sub>{a.universelevel}</sub>
          </span>,
        ];
      }
      case "pi": {
        let iSub = a.format?.indented ? i + 1 : i;
        return (a.format?.indented ? [br, indent(i)] : [])
          .concat([lparen, pi, renderBinding(a.label)])
          .concat(
            a.format?.unannotated
              ? []
              : [colon].concat(
                  renderTerm(
                    a.domain,
                    labels,
                    iSub,
                    ix.push({ case: "pi domain" })
                  )
                )
          )
          .concat(period)
          .concat(
            renderTerm(
              a.codomain,
              labels.push(a.label),
              iSub,
              ix.push({ case: "pi codomain" })
            )
          )
          .concat(rparen);
      }
      case "lambda": {
        let iSub = a.format?.indented ? i + 1 : i;
        return (a.format?.indented ? [br, indent(i)] : [])
          .concat([lparen, lambda, renderBinding(a.label)])
          .concat(
            a.format?.unannotated
              ? []
              : [colon].concat(
                  renderTerm(
                    a.domain,
                    labels,
                    iSub,
                    ix.push({ case: "lambda domain" })
                  )
                )
          )
          .concat([period])
          .concat(
            renderTerm(
              a.body,
              labels.push(a.label),
              iSub,
              ix.push({ case: "lambda body" })
            )
          )
          .concat([rparen]);
      }
      case "let": {
        let iSub = a.format?.indented ? i + 1 : i;
        return (a.format?.indented ? [br, indent(i)] : [])
          .concat([lparen, let_, renderReference(a.label)])
          .concat(
            a.format?.unannotated
              ? []
              : [colon].concat(
                  renderTerm(
                    a.domain,
                    labels,
                    iSub,
                    ix.push({ case: "let domain" })
                  )
                )
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
              labels.push(a.label),
              iSub,
              ix.push({ case: "let body" })
            )
          )
          .concat(rparen);
      }
      case "neutral": {
        let iSub = a.format?.indented ? i + 1 : i;
        let f: JSX.Element;
        switch (a.applicant.case) {
          case "variable": {
            f = renderReference(labels.get(a.applicant.debruijnlevel) as Label);
            break;
          }
          case "hole": {
            f = renderHole(a.applicant.hole, ix);
            break;
          }
        }
        if (a.arguments.size === 0) {
          return (a.format?.indented ? [br, indent(i)] : []).concat([f]);
        } else {
          return (a.format?.indented ? [br, indent(i)] : [])
            .concat(lparen)
            .concat(f)
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
    let classList = ["hole"];
    if (compareTermIx(ix, app.appState.environment.focus))
      classList.push("focussed");
    return (
      <span className={classList.join(" ")} onClick={onClick}>
        {question}
      </span>
    );
  }

  function renderBinding(label: Label): JSX.Element {
    let onChange: React.ChangeEventHandler<HTMLInputElement> = (event) => {
      label.value = event.target.value;
      event.target.value = label.value;
      event.target.style.width = label.value.length + "ch";
      app.setState(app.appState);
    };
    return (
      <input
        className="token label binding"
        onChange={onChange}
        value={label.value}
        width={label.value.length + "ch"}
      ></input>
    );
  }

  function renderReference(label: Label): JSX.Element {
    return <span className="token label reference">{label.value}</span>;
  }

  return <span>{renderTerm(program)}</span>;
}
