import { List } from "immutable";

// Program

export type Program = List<Declaration>

export type Declaration = DefineTerm | DefineData

export type DefineTerm = {
  label: Label,
  signature: Type, 
  value: Term
}

export type DefineData = {
  label: Label,
  parameters: List<Type>,
  constructors: List<Constructor>
}

export type Constructor = {
  label: Label,
  parameters: List<Type>,
  codomain: Type
}

// Block

export type Block = {
  bindings: List<{label: Label, signature: Type, value: Term}>,
  body: Term,
  format: Format
}

// Term

export type Term = Lambda |  Neutral | Match | Hole

export type Lambda = {
  case: "lambda",
  parameters: List<{label: Label, domain: Type}>,
  body: Block,
  format: Format
}

export type Neutral = {
  case: "neutral",
  applicant: Label,
  arguments: List<Term>,
  format: Format
}

export type Match = {
  case: "match",
  argument: Term,
  branches: List<[Pattern, Term]>
}

export type Pattern = {
  label: Label,
  paramaters: List<Label>
}

export type Hole = {
  case: "hole",
  holeId: Symbol,
  weakening: List<Label>,
  substitution: Substitution<Label, Term>,
  format: Format
}

// Type

export type Type = Base | Arrow | Data 

export type Base = {
  case: "basic",
  subcase: "unit" | "boolean" | "natural" | "integer"
}

export type Arrow = {
  case: "arrow",
  arguments: List<Type>,
  codomain: Type
}

export type Data = {
  case: "data",
  label: Label, 
  arguments: List<Type>
}

export type Label = {value: string}

export function freshLabel(): Label {
  return {value: ""}
}

export function freshHole(): Hole {
  const holeId: unique symbol = Symbol()
  return {
    case: "hole",
    holeId,
    weakening: List(),
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

export type Index = List<{}>

// TODO: merge indexing with changes to grammar

// export type Index = List<IndexStep>

// export type IndexStep =
//   | {
//       case: "block",
//       sub:
//         | {
//             case: "binding",
//             i: number,
//             sub: {case: "label"} | {case: "signature"} | {case: "value"},
//             next: Index
//           }
//         | {case: "body"}
//     }
//   | {
//       case: "pi",
//       sub:
//         | {
//             case: "parameter",
//             i: number,
//             sub: {case: "label"} | {case: "signature"}
//           }
//         | {case: "codomain"}
//     }
//   | {
//       case: "lambda",
//       sub:
//         | {
//             case: "parameter",
//             i: number,
//             sub: {case: "label"} | {case: "signature"}
//           }
//         | {case: "body"}
//     }
//   | {
//       case: "neutral",
//       sub:
//         | {case: "applicant"}
//         | {case: "argument", i: number}
//     }