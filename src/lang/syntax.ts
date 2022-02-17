// Module

import ts from "typescript"
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

export type Term = LambdaTerm | NeutralTerm | HoleTerm

export type LambdaTerm = {
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

export type NeutralTerm = {
  case: "neutral",
  applicant: Label,
  args: Term[],
  format: Format
}

export type HoleTerm = {
  case: "hole",
  holeId: Symbol,
  weakening: Label[],
  substitution: Substitution<Label, Term>,
  format: Format
}

// Label

export type Label = {case: "label", value: string, format: Format}

// Fresh

export function freshLabel(): Label {
  return {case: "label", value: "", format: defaultFormat()}
}

export function freshHoleTerm(): HoleTerm {
  const holeId: unique symbol = Symbol()
  return {
    case: "hole",
    holeId,
    weakening: [],
    substitution: [],
    format: defaultFormat()
  }
}

export function freshHoleType(): HoleType {
  const holeId: unique symbol = Symbol()
  return {
    case: "hole",
    holeId,
    format: defaultFormat()
  }
}

// Creates an empty block (0 bindings) with a fresh hole
export function freshBlock(): Block {
  return {
    case: "block",
    bindings: [],
    body: freshHoleTerm(),
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

export type FormatField = "indented" | "unannotated"
export type Format = ts.ESMap<string, boolean>

export function defaultFormat(): Format {
  throw new Error() // TODO: how to make empty ESMap?
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

// Indexing

export function lookupAt<S extends Indexable, T extends Indexable>(source: S, index: Index, gamma: Context): {target: T, gamma: Context} {
  throw new Error("unimplemented")
}

export function replaceAt<S extends Indexable, T extends Indexable>(source: S, index: Index, gamma: Context, replace: (target: T, gamma: Context) => T): S {
  throw new Error("unimplemented")
}

// The kinds of things you can index
export type Indexable = Module | Statement | Constructor | Type | Block | Binding | Term | Parameter | Label

export type IndexStep<I extends Index> = HereIndex | I

export type HereIndex = {case: "here"}

export type Index = ModuleIndex | StatementIndex | ConstructorIndex | TypeIndex | TermIndex | BlockIndex | BindingIndex | ParameterIndex

// ModuleIndex

export type ModuleIndex = IndexStep<{case: "module", i: number, index: StatementIndex}>

// StatementIndex

export type StatementIndex = IndexStep<TermDefinitionIndex | DataDefinitionIndex>
export type TermDefinitionIndex = 
  {
    case: "term definition",
    sub:
      | {case: "label"}
      | {case: "type", index: TypeIndex}
      | {case: "term", index: TermIndex}
  }
export type DataDefinitionIndex =
  {
    case: "data definition",
    sub:
      | {case: "label"}
      | {case: "constructor", i: number, index: ConstructorIndex}
  }

// ConstructorIndex

export type ConstructorIndex =
  IndexStep<{
    case: "constructor", 
    sub: ConstructorSubIndex
  }>
export type ConstructorSubIndex =
  | {case: "label"}
  | {case: "domain", i: number, index: TypeIndex}

// TypeIndex

export type TypeIndex = IndexStep<ArrowTypeIndex | DataTypeIndex>
export type ArrowTypeIndex =
  {
    case: "arrow",
    sub:
      | {case: "domain", i: number, index: TypeIndex} 
      | {case: "codomain", index: TypeIndex}
  }
export type DataTypeIndex =
  {
    case: "data",
    sub:
      | {case: "label"}
  }

// BlockIndex

export type BlockIndex =
  IndexStep<{
    case: "block",
    sub:
      | {case: "binding", i: number, index: BindingIndex}
      | {case: "body", index: TermIndex}
  }>

export type BindingIndex =
  IndexStep<{
    case: "binding",
    sub:
      | {case: "label"}
      | {case: "type", index: TypeIndex}
      | {case: "term", index: TermIndex}
  }>

// TermIndex

export type TermIndex = IndexStep<LambdaTermTermIndex | NeutralTermTermIndex>
export type LambdaTermTermIndex =
  {
  case: "lambda",
  sub:
    | {case: "parameter", i: number, index: ParameterIndex}
    | {case: "body", index: TermIndex}
  }
export type NeutralTermTermIndex =
  {
    case: "neutral",
    sub:
      | {case: "applicant"}
      | {case: "argument", i: number, index: TermIndex}
  }

export type ParameterIndex =
  IndexStep<{
    case: "parameter", 
    sub:
      | {case: "label"}
      | {case: "domain", index: TypeIndex}
  }>

export function pushIndex(moduleIndex: ModuleIndex, index: Index): ModuleIndex {throw new Error()}
  