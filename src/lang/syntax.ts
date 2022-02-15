export type Block = {
  case: "block",
  bindings: Binding[],
  body: Term,
  format: Format
}

export type Binding = {
  case: "binding",
  label: Label,
  signature: Term,
  value: Term
}

export type Term = Universe | Pi | Lambda | Neutral | Hole

export type Universe = {
  case: "universe",
  level: number,
  format: Format
}

export type Pi = {
  case: "pi",
  parameters: Parameter[],
  codomain: Block,
  format: Format
}

export type Lambda = {
  case: "lambda",
  parameters: Parameter[],
  body: Block,
  format: Format
}

export type Parameter = {
  case: "parameter",
  label: Label,
  domain: Term,
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

export type Label = {case: "label", value: string}

export function freshLabel(): Label {
  return {case: "label", value: ""}
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

// Creates an empty block (0 bindings) with a fresh hole
export function freshBlock(): Block {
  return {
    case: "block",
    bindings: [],
    body: freshHole(),
    format: defaultFormat()
  }
}

export function freshParameter(): Parameter {
  return {
    case: "parameter",
    label: freshLabel(),
    domain: freshHole(),
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

export type Context = ContextItem[]
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

export type Mode = 
  | {case: "edit", index: Index}
  | {case: "label", index: Index, label: Label}

export function lookupAt<S extends Indexable, T extends Indexable>(source: S, index: Index, gamma: Context): {target: T, gamma: Context} {
  throw new Error("unimplemented")
}

export function replaceAt<S extends Indexable, T extends Indexable>(source: S, index: Index, gamma: Context, replace: (target: T, gamma: Context) => T): void {
  throw new Error("unimplemented")
}

// The kinds of things you can index
export type Indexable = Block | Binding | Term | Parameter | Label

export type Index = IndexItem[]

export type IndexItem = {
  target: Indexable,
  step: IndexStep
}

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

