import { List } from "immutable";

export type Block = {
  bindings: List<{label: Label, signature: Term, value: Term}>,
  body: Term,
  format: Format
}

export type Term = Universe | Pi | Lambda | Neutral | Hole

export type Lambda = {
  case: "lambda",
  parameters: List<{label: Label, domain: Term}>,
  body: Block,
  format: Format
}

export type Universe = {
  case: "universe",
  level: number,
  format: Format
}

export type Pi = {
  case: "pi",
  parameters: List<{label: Label, domain: Term}>,
  codomain: Block,
  format: Format
}

export type Neutral = {
  case: "neutral",
  applicant: DeBruijn,
  arguments: List<Term>,
  format: Format
}

export type Hole = {
  case: "hole",
  holeId: Symbol,
  weakening: DeBruijn,
  substitution: Substitution<DeBruijn, Term>,
  format: Format
}

// Variables references are DeBruijn levels
export type DeBruijn = number

export type Label = {value: string}

export function freshLabel(): Label {
  return {value: ""}
}

export function freshHole(): Hole {
  const holeId: unique symbol = Symbol()
  return {
    case: "hole",
    holeId,
    weakening: 0,
    substitution: List(),
    format: defaultFormat()
  }
}

// Format

export type Format = {
  indented?: boolean
  unannotated?: boolean
}

export function defaultFormat(): Format {
  return {}
}

// Context

export type Context = List<ContextItem>
export type ContextItem = {
  label: Label,
  signature: Term,
  value: Term | undefined
}

// Substitution

export type Substitution<A, B> = List<SubstitutionItem<A, B>>
export type SubstitutionItem<A, B> = [A, B]

// Index

export type Index = List<IndexStep>

export type IndexStep =
  | {
      case: "block",
      sub:
        | {
            case: "binding",
            i: number,
            sub: {case: "label"} | {case: "signature"} | {case: "value"},
          }
        | {case: "body"},
    }
  | {
      case: "pi",
      sub:
        | {
            case: "parameter",
            i: number,
            sub: {case: "label"} | {case: "signature"}
          }
        | {case: "codomain"}
    }
  | {
      case: "lambda",
      sub:
        | {
            case: "parameter",
            i: number,
            sub: {case: "label"} | {case: "signature"}
          }
        | {case: "body"}
    }
  | {
      case: "neutral",
      sub:
        | {case: "applicant"}
        | {case: "argument", i: number},
    }