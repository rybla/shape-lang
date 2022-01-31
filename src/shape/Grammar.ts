import { List } from "immutable"

// debruijnlevel => [label, type, value?]
export type Context = List<[Label, Term , Term | undefined]>

export type Substitution<A> = List<[A, Term]>;

export type Term = TermPi | TermLambda | TermLet | TermApplication | TermVariable | TermHole

export type TermPi = {
  case: "pi",
  label: Label,
  domain: Term,
  codomain: Term,
  format?: {
    indented: boolean,
    annotated: boolean
  }
}

export type TermLambda = {
  case: "lambda",
  label: Label,
  domain: Term,
  body: Term,
  format?: {
    indented: boolean,
    annotated: boolean
  }
}

export type TermLet = {
  case: "let",
  label: Label,
  domain: Term,
  argument: Term,
  body: Term,
  format?: {
    indented: boolean,
    annotated: boolean
  }
}

export type TermApplication = {
  case: "application",
  applicant: Term,
  argument: Term,
  format?: {
    indented: boolean
  }
}

export type TermVariable = {
  case: "variable",
  debruijnlevel: DeBruijnLevel,
  format?: {
    // TODO
  }
}

export type TermHole = {
  case: "hole",
  hole: Hole,
  weakening: DeBruijnLevel,
  substitution: Substitution<DeBruijnLevel>
  format?: {
    // TODO
  }
}

// A term index specifies a node in a Term AST.
export type TermIx = {} // TODO

// DeBruijn level
export type DeBruijnLevel = number;

// Variable label
export type Label = {value: string};

// Hole Id
export type Hole = {holeId: HoleId}
export type HoleId = number;
