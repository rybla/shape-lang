import { List } from "immutable"

// Term

// export type Term
//   = {case: "uni"}
//   | {case: "pi", label: Label, dom: Term, cod: Term}
//   | {case: "lam", label: Label, dom: Term, bod: Term}
//   | {case: "let", label: Label, dom: Term, arg: Term, bod: Term}
//   | {case: "app", app: Term, arg: Term}
//   | {case: "var", dbl: Dbl}
//   | {case: "hole", id: HoleId, subs : List<Term>, weak : number}
export type Term = TermUniverse | TermPi | TermLambda | TermLet | TermNeutral

// Universe

export type TermUniverse = {
  case: "universe",
  universelevel: UniverseLevel;
  format: {
    indented?: boolean
  }
}

export type UniverseLevel = number;

// Pi

export type TermPi = {
  case: "pi",
  label: Label,
  domain: Term,
  codomain: Term,
  format: {
    indented?: boolean,
    unannotated?: boolean
  }
}

// Lambda

export type TermLambda = {
  case: "lambda",
  label: Label,
  domain: Term,
  body: Term,
  format: {
    indented?: boolean,
    unannotated?: boolean
  }
}

// Let

export type TermLet = {
  case: "let",
  label: Label,
  domain: Term,
  argument: Term,
  body: Term,
  format: {
    indented?: boolean,
    unannotated?: boolean
  }
}

// Neutral

export type TermNeutral = {
  case: "neutral",
  applicant: {case: "variable", debruijnlevel: DeBruijnLevel} | {case: "hole", hole: Hole},
  arguments: List<Term>,
  format: {
    indented?: boolean
  }
}

export type DeBruijnLevel = number;

export type Label = {value: string};

// Hole

export type Hole = {
  case: "hole",
  holesymbol: HoleSymbol,
  weakening: DeBruijnLevel,
  substitution: Substitution<DeBruijnLevel>
  format: {
    indented?: boolean
  }
}

export type HoleSymbol = Symbol

export type HoleId = number;

export function makeHole(holesymbol: HoleSymbol, weakening: DeBruijnLevel = 0, substitution: Substitution<DeBruijnLevel> = List()): Hole {
  return {case: "hole", holesymbol, weakening, substitution, format: {}};
}

export function freshHole(): Hole {
  const holesymbol: unique symbol = Symbol();
  return makeHole(holesymbol);
}

export function freshHoleTerm(): Term {
  return {case: "neutral", applicant: {case: "hole", hole: freshHole()}, arguments: List(), format: {}}
}



// Context

// debruijnlevel => [label, type, value?]
export type Context = List<ContextItem>
export type ContextItem = [Label, Term, Term | undefined]

// Substitution

// A => Term
export type Substitution<A> = List<SubstitutionItem<A>>
export type SubstitutionItem<A> = [A , Term]

// Term Index

// A term index specifies a node in a Term AST.
export type TermIx = List<TermIxStep>;
export type TermIxStep
  = {case: "pi domain"} | {case: "pi codomain"}
  | {case: "lambda domain"} | {case: "lambda body"}
  | {case: "let domain"} | {case: "let argument"} | {case: "let body"}
  | {case: "application argument", iArg: number}

  export function compareTermIx(ix1: TermIx, ix2: TermIx): boolean {
    if (ix1.size !== ix2.size) return false;
    for (let i = 0; i < ix1.size; i ++) {
      let ix1_i = ix1.get(i) as TermIxStep;
      let ix2_i = ix2.get(i) as TermIxStep;
      switch (ix1_i.case) {
        case "application argument": 
          if (!(ix2_i.case === "application argument" && ix1_i.iArg === ix2_i.iArg))
            return false;
          break;
        default:
          if (!(ix1_i.case === ix2_i.case))
            return false;
      }
    }
    return true;
  }

// Show

export function showTerm(a: Term, labels: List<Label> = List()): string {
  switch (a.case) {
    case "universe": return `U[${a.universelevel}]`;
    case "pi": return `(Π ${a.label.value} : ${showTerm(a.domain, labels)} . ${showTerm(a.codomain, labels.push(a.label))})`;
    case "lambda": return `(λ ${a.label.value} : ${showTerm(a.domain, labels)} . ${showTerm(a.body, labels.push(a.label))})`;
    case "let": return `(let ${a.label.value} : ${showTerm(a.domain, labels)} = ${showTerm(a.argument, labels)} in ${showTerm(a.body, labels.push(a.label))})`;
    case "neutral": {
      var f;
      switch (a.applicant.case) {
        case "variable": f = (labels.get(a.applicant.debruijnlevel) as Label).value; break;
        case "hole": f = "?"; break;
      }
      if (a.arguments.size === 0) return f;
      else return `(${f} ${a.arguments.map(b => showTerm(b, labels)).reduce((a, b) => `${a} ${b}`)})`;
    }
  }
}