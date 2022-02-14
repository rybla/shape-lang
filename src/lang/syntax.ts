export type Block = {
  bindings: {label: Label, signature: Term, value: Term}[],
  body: Term,
  format: Format
}

export type Term = Universe | Pi | Lambda | Neutral | Hole

export type Universe = {
  case: "universe",
  level: number,
  format: Format
}

export type Pi = {
  case: "pi",
  parameters: {label: Label, domain: Term}[],
  codomain: Block,
  format: Format
}

export type Lambda = {
  case: "lambda",
  parameters: {label: Label, domain: Term}[],
  body: Block,
  format: Format
}

export type Neutral = {
  case: "neutral",
  applicant: DeBruijn,
  arguments: Term[],
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
    substitution: [],
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

export type Context = [ContextItem]
export type ContextItem = {
  label: Label,
  signature: Term,
  value: Term | undefined
}

// Substitution

export type Substitution<A, B> = SubstitutionItem<A, B>[]
export type SubstitutionItem<A, B> = {
  key: A,
  value: B
}

// Mode

export type Mode = {
  index: Index,
  sub:
    | {case: "edit"}
    | {case: "label", label: Label}
}


export type Index = IndexStep[]

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

export function modifyInBlock<A>(block: Block, index: Index, modifier: (a: A) => A): void {
  throw new Error()
}

export function modifyInTerm<A>(term: Term, index: Index, modifier: (a: A) => A): void {
  throw new Error()
}