import { List } from "immutable";
import React, { CSSProperties, MouseEventHandler } from "react";
import App, { app } from "./App";
import { collectContext } from "./shape/Typing";
import {
  Label,
  Term,
  Context,
  Hole,
  TermIx,
  compareTermIx,
  TermNeutral,
} from "./shape/Grammar";
import {
  applyTransformation,
  placeLambda,
  placeLet,
  placeVariable,
  placePi,
  placeUniverse,
  placeHole,
  toggleIndent,
  enterBinding,
} from "./shape/Transformation";
import { Environment } from "./shape/Environment";
import { help } from "./Help";

export function renderApp(): JSX.Element {
  // onKeyPress={(event) => console.log("hello world")}
  return (
    <div className="App">
      {renderWorkspace()}
      <br></br>
      {help}
      <div className="sidebar">
        {renderPalette()}
        {renderConsole()}
        {renderCommandLine()}
      </div>
    </div>
  );
}

function renderWorkspace(): JSX.Element {
  return (
    <div className="workspace">
      {renderProgram(app.appState.environment.program)}
    </div>
  );
}

function renderPalette(): JSX.Element {
  let options: List<{
    label: string;
    onClick: MouseEventHandler<HTMLDivElement>;
  }> = List([
    {
      label: "universe",
      onClick: (event) => applyTransformation(placeUniverse),
    },
    {
      label: "pi",
      onClick: (event) => applyTransformation(placePi),
    },
    {
      label: "lambda",
      onClick: (event) => applyTransformation(placeLambda),
    },
    {
      label: "let",
      onClick: (event) => applyTransformation(placeLet),
    },
  ]);
  let env = app.appState.environment;
  let context: Context = collectContext(
    env.program,
    app.appState.mode.focus
  )[0];
  context.forEach((item, dbl) => {
    if (item[0].value === "_" || item[0].value.length === 0) return;
    options = options.push({
      label: "neutral " + item[0].value,
      onClick: (event) => {
        applyTransformation(placeVariable(dbl));
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

function renderConsole(): JSX.Element {
  return (
    <div className="console">
      {}
      {}
    </div>
  );
}

function renderCommandLine(): JSX.Element {
  return (
    <div className="commandline">
      {}
      {}
    </div>
  );
}

function renderProgram(program: Term): JSX.Element {
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
    return <span style={{ display: "inline-block", width: i + "em" }}></span>;
  }

  let parenCount = 0;

  function renderTerm(
    a: Term,
    labels: List<Label> = List(),
    i: number = 0,
    ix: TermIx = List()
  ): JSX.Element[] {
    let focussed: boolean = compareTermIx(ix, app.appState.mode.focus);
    let focussedClass: string = focussed ? ` focussed` : "";

    function makeParens(): [JSX.Element, JSX.Element] {
      let parenId = parenCount;
      let style: CSSProperties = focussed ? { color: "red" } : {};
      const onMouseEnter: MouseEventHandler<HTMLSpanElement> = (event) => {
        let el = document.getElementById("lparen" + parenId);
        let er = document.getElementById("rparen" + parenId);
        el?.setAttribute("style", "color: orange");
        er?.setAttribute("style", "color: orange");
      };
      const onMouseLeave: MouseEventHandler<HTMLSpanElement> = (event) => {
        let el = document.getElementById("lparen" + parenId);
        let er = document.getElementById("rparen" + parenId);
        el?.setAttribute("style", "");
        er?.setAttribute("style", "");
      };
      let lpar = (
        <span
          id={"lparen" + parenId}
          className="token punctuation left-paren"
          style={style}
          onMouseEnter={onMouseEnter}
          onMouseLeave={onMouseLeave}
        >
          (
        </span>
      );
      let rpar = (
        <span
          id={"rparen" + parenId}
          className="token punctuation reft-paren"
          style={style}
          onMouseEnter={onMouseEnter}
          onMouseLeave={onMouseLeave}
        >
          )
        </span>
      );
      parenCount++;
      return [lpar, rpar];
    }

    // if (a.case !== "neutral") {
    //   a.format = {
    //     indented: true,
    //     annotated: true,
    //   };
    // }

    switch (a.case) {
      case "universe": {
        return (a.format?.indented ? [br, indent(i)] : []).concat([
          <span className={"term universe" + focussedClass}>
            U<sub>{a.universelevel}</sub>
          </span>,
        ]);
      }
      case "pi": {
        let [lpar, rpar] = makeParens();
        return (a.format?.indented ? [br, indent(i)] : [])
          .concat([
            lpar,
            <span className={"token punctuation pi" + focussedClass}>Π</span>,
            renderBinding(a.label, ix),
          ])
          .concat(
            a.format?.unannotated
              ? []
              : [colon].concat(
                  renderTerm(
                    a.domain,
                    labels,
                    i + 1,
                    ix.push({ case: "pi domain" })
                  )
                )
          )
          .concat(period)
          .concat(
            renderTerm(
              a.codomain,
              labels.push(a.label),
              i + 1,
              ix.push({ case: "pi codomain" })
            )
          )
          .concat(rpar);
      }
      case "lambda": {
        let [lpar, rpar] = makeParens();
        return (a.format?.indented ? [br, indent(i)] : [])
          .concat([
            lpar,
            <span className={"token punctuation lambda" + focussedClass}>
              λ
            </span>,
            renderBinding(a.label, ix),
          ])
          .concat(
            a.format?.unannotated
              ? []
              : [colon].concat(
                  renderTerm(
                    a.domain,
                    labels,
                    i + 1,
                    ix.push({ case: "lambda domain" })
                  )
                )
          )
          .concat([period])
          .concat(
            renderTerm(
              a.body,
              labels.push(a.label),
              i + 1,
              ix.push({ case: "lambda body" })
            )
          )
          .concat([rpar]);
      }
      case "let": {
        console.log(i);
        let [lpar, rpar] = makeParens();
        return (a.format?.indented ? [br, indent(i)] : [])
          .concat([
            lpar,
            <span className={"token punctuation let" + focussedClass}>
              let
            </span>,
            renderBinding(a.label, ix),
          ])
          .concat(
            a.format?.unannotated
              ? []
              : [colon].concat(
                  renderTerm(
                    a.domain,
                    labels,
                    i + 2,
                    ix.push({ case: "let domain" })
                  )
                )
          )
          .concat(assignment)
          .concat(
            renderTerm(
              a.argument,
              labels,
              i + 2,
              ix.push({ case: "let argument" })
            )
          )
          .concat(in_)
          .concat(
            renderTerm(
              a.body,
              labels.push(a.label),
              i + 1,
              ix.push({ case: "let body" })
            )
          )
          .concat(rpar);
      }
      case "neutral": {
        let [lpar, rpar] = makeParens();
        let f: JSX.Element;
        switch (a.applicant.case) {
          case "variable": {
            f = renderReference(
              labels.get(a.applicant.debruijnlevel) as Label,
              focussedClass
            );
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
            .concat(lpar)
            .concat([f, space])
            .concat(
              a.arguments
                .map((a, iArg) =>
                  renderTerm(
                    a,
                    labels,
                    i + 1,
                    ix.push({ case: "application argument", iArg })
                  )
                )
                .toArray()
                .reduce((a, b) => a.concat([space], b))
            )
            .concat(rpar);
        }
      }
    }
  }

  function renderHole(hole: Hole, ix: TermIx): JSX.Element {
    let onClick: MouseEventHandler<HTMLSpanElement> = (event) => {
      app.appState.mode = {
        case: "term",
        focus: ix,
      };
      app.setState(app.appState);
    };
    let classList = ["hole"];
    if (compareTermIx(ix, app.appState.mode.focus)) classList.push("focussed");
    return (
      <span className={classList.join(" ")} onClick={onClick}>
        {question}
      </span>
    );
  }

  function renderBinding(label: Label, ix: TermIx): JSX.Element {
    let s = label.value.length > 0 ? label.value : "_";
    if (
      app.appState.mode.case === "label" &&
      compareTermIx(app.appState.mode.focus, ix)
    ) {
      return <span className="token label binding focussed">{s}</span>;
    } else {
      return <span className="token label binding">{s}</span>;
    }
  }

  function renderReference(label: Label, focussedClass: string): JSX.Element {
    let s = label.value.length > 0 ? label.value : "_";
    return <span className={"token label reference" + focussedClass}>{s}</span>;
  }

  return <span>{renderTerm(program)}</span>;
}

type Direction = "ArrowUp" | "ArrowDown" | "ArrowLeft" | "ArrowRight";
export const ArrowDirections: string[] = [
  "ArrowUp",
  "ArrowDown",
  "ArrowLeft",
  "ArrowRight",
];

export function moveTermIx(ix: TermIx, d: Direction): TermIx | undefined {
  switch (d) {
    case "ArrowUp": {
      if (ix.size > 0) return ix.pop();
      else return undefined;
    }
    case "ArrowDown": {
      return moveTermIxDown(ix);
    }
    case "ArrowLeft": {
      let ixParent = ix.pop();
      let ixStep = ix.last();
      if (ixStep) {
        switch (ixStep.case) {
          // pi
          case "pi domain": {
            return ixParent;
          }
          case "pi codomain": {
            return ixParent.push({ case: "pi domain" });
          }
          // lambda
          case "lambda domain": {
            return ixParent;
          }
          case "lambda body": {
            return ixParent.push({ case: "lambda domain" });
          }
          // let
          case "let domain": {
            return ixParent;
          }
          case "let argument": {
            return ixParent.push({ case: "let domain" });
          }
          case "let body": {
            return ixParent.push({ case: "let argument" });
          }
          // application
          case "application argument": {
            if (ixStep.iArg > 0) {
              return ix.push({
                case: "application argument",
                iArg: Math.max(0, ixStep.iArg - 1),
              });
            } else {
              return ixParent;
            }
          }
        }
      }
      return undefined;
    }
    case "ArrowRight": {
      let a = collectContext(app.appState.environment.program, ix)[1];
      switch (a.case) {
        case "universe": {
          return moveTermIxForceRight(ix);
        }
        case "pi": {
          return moveTermIx(ix, "ArrowDown");
        }
        case "lambda": {
          return moveTermIx(ix, "ArrowDown");
        }
        case "let": {
          return moveTermIx(ix, "ArrowDown");
        }
        case "neutral": {
          if (a.arguments.size > 0)
            return ix.push({ case: "application argument", iArg: 0 });
          else if (ix.size > 0) return moveTermIxForceRight(ix);
          else return undefined;
        }
      }
    }
  }
}

function moveTermIxForceRight(ix: TermIx): TermIx | undefined {
  let ixStep = ix.last();
  if (ixStep) {
    switch (ixStep.case) {
      case "pi domain": {
        return ix.pop().push({ case: "pi codomain" });
      }
      case "pi codomain": {
        return moveTermIxForceRight(ix.pop());
      }
      case "lambda domain": {
        return ix.pop().push({ case: "lambda body" });
      }
      case "lambda body": {
        return moveTermIxForceRight(ix.pop());
      }
      case "let domain": {
        return ix.pop().push({ case: "let argument" });
      }
      case "let body": {
        return moveTermIxForceRight(ix.pop());
      }
    }
  } else return undefined;
}

function moveTermIxDown(ix: TermIx): TermIx | undefined {
  let a = collectContext(app.appState.environment.program, ix)[1];
  switch (a.case) {
    case "pi": {
      return ix.push({ case: "pi domain" });
    }
    case "lambda": {
      return ix.push({ case: "lambda domain" });
    }
    case "let": {
      return ix.push({ case: "let domain" });
    }
    case "neutral": {
      if (a.arguments.size > 0)
        return ix.push({ case: "application argument", iArg: 0 });
      else return undefined;
    }
  }
  return undefined;
}

function moveTermIxDownmost(ix: TermIx): TermIx {
  let ixDown = moveTermIxDown(ix);
  if (ixDown) return moveTermIxDownmost(ixDown);
  else return ix;
}

window.addEventListener("keydown", (event) => {
  event.preventDefault();

  let focus: TermIx = app.appState.mode.focus;

  // arrow movement
  if (ArrowDirections.includes(event.key)) {
    console.log(event.key, focus.toArray());
    let focusNew = moveTermIx(focus, event.key as Direction);
    if (focusNew) {
      app.appState.mode = {
        case: "term",
        focus: focusNew,
      };
      app.setState(app.appState);
    }
  } else if (app.appState.mode.case === "label") {
    let label = app.appState.mode.label;
    switch (event.key) {
      case "Backspace":
        label.value = label.value.slice(0, label.value.length - 1);
        app.setState(app.appState);
        break;
      case "Enter":
        app.appState.mode = {
          case: "term",
          focus: app.appState.mode.focus,
        };
        app.setState(app.appState);
        break;
      case "Tab":
        app.appState.mode = {
          case: "term",
          focus: app.appState.mode.focus,
        };
        let focusNew1 = moveTermIx(focus, "ArrowDown");
        if (focusNew1) {
          app.appState.mode = {
            case: "term",
            focus: focusNew1,
          };
          let focusNew2 = moveTermIx(focus, "ArrowRight");
          if (focusNew2) {
            app.appState.mode = {
              case: "term",
              focus: focusNew2,
            };
          }
          app.setState(app.appState);
        }
        break;
      default: {
        console.log(event.key);
        if (!["Meta", "Shift", "Alt"].includes(event.key)) {
          label.value = label.value + event.key;
          app.setState(app.appState);
        }
        break;
      }
    }
  } else if (app.appState.mode.case === "term") {
    switch (event.key) {
      case "u":
        applyTransformation(placeUniverse);
        break;
      case "p":
        applyTransformation(placePi);
        break;
      case "l":
        applyTransformation(placeLambda);
        break;
      case "=":
        applyTransformation(placeLet);
        break;
      case "d":
        applyTransformation(placeHole);
        break;
      case "x":
        console.log("entering binding");
        applyTransformation(enterBinding);
        console.log(app.appState.mode);
        break;
      case "Backspace":
        applyTransformation(placeHole);
        break;
      case "Enter":
        applyTransformation(toggleIndent);
        break;
      case "Tab":
        let focusNew = moveTermIx(
          focus,
          event.shiftKey ? "ArrowLeft" : "ArrowRight"
        );
        if (focusNew) {
          app.appState.mode = {
            case: "term",
            focus: focusNew,
          };
        }
        app.setState(app.appState);
        break;
      default:
        break;
    }
  }
});
