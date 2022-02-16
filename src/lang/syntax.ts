// Module

export type Module = {
  case: "module",
  statements: Statement[],
  format: Format
}

// Statement

export type Statement = TypeDefinition | DataDefinition | TermDefinition

export type TypeDefinition = {
  case: "type definition",
  label: Label,
  type: Type,
  format: Format
}

export type DataDefinition = {
  case: "data definition",
  label: Label,
  constructors: Constructor[],
  format: Format
}

export type Constructor = {
  case: "constructor",
  label: Label,
  parameters: Parameter[],
  format: Format
}

export type TermDefinition = {
  case: "term definition",
  label: Label,
  type: Type,
  block: Term,
  format: Format
}

// Type

export type Type = ArrowType | DataType

export type ArrowType = {
  case: "arrow",
  parameters: Type[],
  codomain: Type,
  format: Format
}

export type DataType = {
  case: "data",
  label: Label,
  format: Format
}

// Block

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
  value: Term,
  format: Format
}

// Term

export type Term = Lambda | Neutral | Hole

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
  applicant: Label,
  args: Term[],
  format: Format
}

export type Hole = {
  case: "hole",
  holeId: Symbol,
  weakening: Label[],
  substitution: Substitution<Label, Term>,
  format: Format
}

// Label

export type Label = {case: "label", value: string}

// Fresh

export function freshLabel(): Label {
  return {case: "label", value: ""}
}

export function freshHole(): Hole {
  const holeId: unique symbol = Symbol()
  return {
    case: "hole",
    holeId,
    weakening: [],
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
export type Indexable = Module | Statement | Constructor | Block | Binding | Term | Parameter | Label

export type Index = IndexItem[]

export type IndexItem = {
  target: Indexable,
  step: IndexStep
}

export type IndexStep =
  | {
      case: "module",
      sub: {case: "statement", i: number}
    }
  | {
      case: "type definition",
      sub: {case: "label"} | {case: "type"}
    }
  | {
      case: "data definition",
      sub: {case: "label"} | {case: "constructor", i: number}
    }
  | {
      case: "constructor",
      sub: {case: "label"} | {case: "paramater", i: number}
    }
  | {
      case: "term definition",
      sub: {case: "label"} | {case: "type"} | {case: "block"}
    }
  | {
      case: "block",
      sub: {case: "binding", i: number} | {case: "body"},
    }
  | {
      case: "binding",
      sub: {case: "label"} | {case: "signature"} | {case: "value"},
    }
  | {
      case: "pi",
      sub: {case: "parameter", i: number} | {case: "codomain"}
    }
  | {
      case: "lambda",
      sub: {case: "parameter", i: number} | {case: "body"}
    }
  | {
      case: "parameter",
      sub: {case: "label"} | {case: "signature"}
    }
  | {
      case: "neutral",
      sub: {case: "applicant"} | {case: "argument", i: number},
    }

