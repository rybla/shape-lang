import { Map } from "immutable"

// Syntax

export type Syntax = 
  | Block
  | Definition
  | Constructor
  | Type
  | Term
  | Parameter
  | Binding
  | Name
  | Reference

// Block

export type Block = {
  case: "block",
  definitions: Definition[],
  body: Term
}

// Definition

export type Definition = DataDefinition | TermDefinition

export type DataDefinition = {
  case: "data definition",
  id: Id,
  constructors: Constructor[]
}

export type Constructor = {
  case: "constructor",
  binding: UniqueBinding,
  parameters: Parameter[]
}

export const typeOfConstructor = (dataRef: Reference, constructor: Constructor): Type => 
  ({
      case: "arrow",
      parameters: constructor.parameters,
      output: {case: "data", reference: dataRef}
  })

export type TermDefinition = {
  case: "term definition",
  binding: UniqueBinding,
  type: Type,
  term: Term
}

// Type

export type Type = ArrowType | DataType | HoleType

export type ArrowType = {
  case: "arrow",
  parameters: Parameter[],
  output: DataType | HoleType
}

export type DataType = {
  case: "data",
  reference: Reference
}

export type HoleType = {
  case: "hole",
  holeId: HoleId
}

// Term

export type Term = LambdaTerm | NeutralTerm | MatchTerm | HoleTerm

export type LambdaTerm = {
  case: "lambda",
  ids: Id[],
  block: Block
}

export type NeutralTerm = {
  case: "neutral",
  reference: Reference
  args: Term[]
}

export type MatchTerm = {
  case: "match",
  reference: Reference,
  term: Term,
  cases: Case[]
}

export type Case = {
  case: "case",
  ids: Id[],
  block: Block
}

export type HoleTerm = {
  case: "hole",
  holeId: Symbol,
  weakening: Weakening,
  substitution: Substitution<Name, Term>
}

// Parameter

export type Parameter = {
  case: "parameter"
  name: Name,
  type: Type
}

// Binding and UniqueBinding

export type Binding = {
  case: "binding",
  id: Id
}

export type UniqueBinding = {
  case: "unique binding",
  Name: Name,
  id: Id
}

// Reference

export type Reference = {
  case: "reference",
  id: Id
}

// Name

export type Name = string

// Id (unique)

export type Id = Symbol

// HoleId (unique)

export type HoleId = Symbol

// Fresh

// export function freshName(): Name {
//   return ""
// }

// export function freshHoleTerm(): HoleTerm {
//   const holeId: unique symbol = Symbol()
//   return {
//     case: "hole",
//     holeId,
//     weakening: [],
//     substitution: []
//   }
// }

// export function freshHoleType(): HoleType {
//   const holeId: unique symbol = Symbol()
//   return {
//     case: "hole",
//     holeId
//   }
// }

// // Creates an empty block (0 bindings) with a fresh hole
// export function freshBlock(): Block {
//   return {
//     case: "block",
//     definitions: [],
//     body: freshHoleTerm()
//   }
// }

// export function freshParameter(domain: Type): Parameter {
//   return {
//     case: "parameter",
//     name: freshName(),
//     domain
//   }
// }

// Context

export type Context = Map<Name, Type>

// Weakening

export type Weakening = Id[]

// Substitution

export type Substitution<A, B> = SubstitutionItem<A, B>[]
export type SubstitutionItem<A, B> = {
  key: A,
  value: B
}

