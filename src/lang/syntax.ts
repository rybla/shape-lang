// Module

import { Map } from "immutable"

export type Module = {
  case: "module",
  statements: Statement[],
  format: Format
}

// Statement

export type Statement = DataDefinition | TermDefinition

export type DataDefinition = {
  case: "data definition",
  label: Label,
  constructors: Constructor[],
  format: Format
}

export type Constructor = {
  case: "constructor",
  label: Label,
  domains: Type[],
  format: Format
}

export const typeOfConstructor = (dataLabel: Label, constructor: Constructor): Type => 
  ({
      case: "arrow",
      domains: constructor.domains,
      codomain: {case: "data", label: dataLabel, format: defaultFormat()},
      format: defaultFormat()
  })

export type TermDefinition = {
  case: "term definition",
  label: Label,
  type: Type,
  block: Block,
  format: Format
}

// Type

export type Type = ArrowType | DataType | HoleType

export type ArrowType = {
  case: "arrow",
  domains: Type[],
  codomain: Type,
  format: Format
}

export type DataType = {
  case: "data",
  label: Label,
  format: Format
}

export type HoleType = {
  case: "hole",
  holeId: Symbol,
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
  type: Type,
  term: Term,
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
  domain: Type,
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

export function freshParameter(domain: Type): Parameter {
  return {
    case: "parameter",
    label: freshLabel(),
    domain,
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

export type Context = Map<Label, Type>

// Substitution

export type Substitution<A, B> = SubstitutionItem<A, B>[]
export type SubstitutionItem<A, B> = {
  key: A,
  value: B
}

// Mode

export type Mode = 
  | {case: "edit"}
  | {case: "label", label: Label}

// The kinds of things you can index
export type Indexable = Module | Statement | Constructor | Block | Binding | Term | Parameter | Label

// export type Index = IndexStep[]

export type ModuleIndex =
  {case: "module", i: number, index: StatementIndex}

export type StatementIndex =
  | {
      case: "type definition",
      sub:
        | {case: "label"}
        | {case: "type", index: TypeIndex}
    }
  | {
      case: "data definition",
      sub:
        | {case: "label"}
        | {case: "constructor", i: number, index: ConstructorIndex}
    }

export type ConstructorIndex =
  {
    case: "constructor", 
    sub: 
      | {case: "label"}
      | {case: "domain", i: number, index: TypeIndex}
  }

export type TypeIndex = 
  | {
      case: "arrow",
      sub:
        | {case: "domain", i: number, index: TypeIndex} 
        | {case: "codomain", index: TypeIndex}
    }
  | {
      case: "data",
      sub:
        | {case: "label"}
    }

export type BlockIndex =
  {
    case: "block",
    sub:
      | {case: "binding", i: number, index: BindingIndex}
      | {case: "body", index: TermIndex}
  }

export type BindingIndex = 
  {
    case: "binding",
    sub:
      | {case: "label"}
      | {case: "type", index: TypeIndex}
      | {case: "term", index: TermIndex}
  }

export type TermIndex = 
  | {
      case: "lambda",
      sub:
        | {case: "parameter", i: number, index: ParameterIndex}
        | {case: "body", index: TermIndex}
    }
  | {
      case: "neutral",
      sub:
        | {case: "applicant"}
        | {case: "argument", i: number, index: TermIndex}
    }

export type ParameterIndex =
  {
    case: "parameter", 
    sub:
      | {case: "label"}
      | {case: "domain", index: TypeIndex}
  }

  