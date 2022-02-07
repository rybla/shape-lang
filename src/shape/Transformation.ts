// A transformation is a function that inputs a Term and outputs a Term.

import { Context, ContextItem, DeBruijnLevel, freshHoleTerm, Label, showTerm, Term, TermIx, TermIxStep } from "./Grammar";
import { Environment } from "./Environment";
import { List } from "immutable";
import { infer, inferParameters } from "./Typing";
import App, { app } from "../App";

type Transformation = (env: Environment, gamma: Context, alpha: Term, a: Term) => Term | undefined

export function applyTransformation(trans: Transformation) {
  let envNew = tryTransformation(app.appState.environment, trans);
  if (envNew) {
    console.log("transformation success");
    let appState = app.appState;
    appState.environment = envNew;
    app.setState(appState);
  } else {
    console.log("transformation failure");
  }
}

export function tryTransformation(env: Environment, trans: Transformation): Environment | undefined {
  console.log("tryTransformation", showTerm(env.program), env.focus.toArray());
  function go(ix: TermIx, gamma: Context, a: Term): Term | undefined {
    if (ix.size === 0 ) {
      return trans(env, gamma, infer(gamma, a), a);
    } else {
      let step = ix.first() as TermIxStep;
      ix = ix.shift();
      switch (a.case) {
        case "pi": {
          switch (step.case) {
            case "pi domain": {
              let domain = go(ix, gamma, a.domain);
              if (domain) return {case: "pi", label: a.label, domain, codomain: a.codomain , format: a.format}
              else return undefined;
            }
            case "pi codomain": {
              let codomain = go(ix, gamma.push([a.label, a.domain, undefined]), a.codomain);
              if (codomain) return {case: "pi", label: a.label, domain: a.domain, codomain, format: a.format};
              else return undefined;
            }
            default: return undefined;
          }
        }
        case "lambda": {
          switch (step.case) {
            case "lambda domain": {
              let domain = go(ix, gamma, a.domain);
              if (domain) return {case: "lambda", label: a.label, domain, body: a.body, format: a.format};
              else return undefined;
            }
            case "lambda body": {
              let body = go(ix, gamma.push([a.label, a.domain, undefined]), a.body);
              if (body) return {case: "lambda", label: a.label, domain: a.domain, body, format: a.format};
              else return undefined;
            }
            default: return undefined;
          }
        }
        case "let": {
          switch (step.case) {
            case "let domain": {
              let domain = go(ix, gamma, a.domain);
              if (domain) return {case: "let", label: a.label, domain, argument: a.argument, body: a.body, format: a.format};
              return undefined;
            }
            case "let argument": {
              let argument = go(ix, gamma, a.argument);
              if (argument) return {case: "let", label: a.label, domain: a.domain, argument, body: a.body, format: a.format};
              else return undefined;
            }
            case "let body": {
              let body = go(ix, gamma.push([a.label, a.domain, a.argument]), a.body);
              if (body) return {case: "let", label: a.label, domain: a.domain, argument: a.argument, body, format: a.format};
              else return undefined;
            }
            default: return undefined;
          }
        }
        case "neutral": {
          switch (step.case) {
            case "application argument": {
              let argument = go(ix, gamma, a.arguments.get(step.iArg) as Term);
              if (argument) return {case: "neutral", applicant: a.applicant, arguments: a.arguments.set(step.iArg, argument), format: a.format};
              else return undefined;
            }
          }
        }
      }
    }
  }
  let programNew = go(env.focus, List(), env.program);
  if (programNew) return env.set("program", programNew);
}

// transformations

export function freshLabel(): Label {
  return ({value: "x"});
}

export const placeUniverse: Transformation = (env, gamma, alpha, a) => {
  return {case: "universe", universelevel: 0, format: {}};
}

export const placePi: Transformation = (env, gamma, alpha, a) => {
  return {case: "pi", label: freshLabel(), domain: freshHoleTerm(), codomain: freshHoleTerm(), format: {}};
}

export const placeLambda: Transformation = (env, gamma, alpha, a) => {
  return {case: "lambda", label: freshLabel(), domain: freshHoleTerm(), body: freshHoleTerm(), format: {}};
}

export const placeLet: Transformation = (env, gamma, alpha, a) => {
  return {case: "let", label: freshLabel(), domain: freshHoleTerm(), argument: freshHoleTerm(), body: freshHoleTerm(), format: {}};
}

export function placeVariable(dbl: DeBruijnLevel): Transformation {
  return (env, gamma, alpha, a) => {
    let beta = (gamma.get(dbl) as ContextItem)[1];
    let params = inferParameters(beta);
    return {case: "neutral", applicant: {case: "variable", debruijnlevel: dbl}, arguments: params.map(param => freshHoleTerm()), format: {}}
  }
}

// i.e. dig
export const placeHole: Transformation = (env, gamma, alpha, a) => {
  return freshHoleTerm();
}

export const toggleIndent: Transformation = (env, gamma, alpha, a) => {
  a.format.indented = !a.format.indented;
  return a;
}