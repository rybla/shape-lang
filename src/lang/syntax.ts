import ts from "typescript"
import { Map } from "immutable"

// Syntax

export type Syntax = Module | Statement | Constructor | Type | Block | Binding | Term | Parameter | Label

// Module

export type Module = {
  case: "module",
  statements: Statement[]
}

// Statement

export type Statement = DataDefinition | TermDefinition

export type DataDefinition = {
  case: "data definition",
  label: Label,
  constructors: Constructor[]
}

export type Constructor = {
  case: "constructor",
  label: Label,
  domains: Type[]
}

export const typeOfConstructor = (dataLabel: Label, constructor: Constructor): Type => 
  ({
      case: "arrow",
      domains: constructor.domains,
      codomain: {case: "data", label: dataLabel}
  })

export type TermDefinition = {
  case: "term definition",
  label: Label,
  type: Type,
  term: Term
}

// Type

export type Type = ArrowType | DataType | HoleType

export type ArrowType = {
  case: "arrow",
  domains: Type[],
  codomain: DataType | HoleType
}

export type DataType = {
  case: "data",
  label: Label
}

export type HoleType = {
  case: "hole",
  holeId: Symbol
}

// Block

export type Block = {
  case: "block",
  bindings: Binding[],
  body: Term
}

export type Binding = {
  case: "binding",
  label: Label,
  type: Type,
  term: Term
}

// Term

export type Term = LambdaTerm | NeutralTerm | HoleTerm

export type LambdaTerm = {
  case: "lambda",
  parameters: Parameter[],
  body: Block
}

export type Parameter = {
  case: "parameter",
  label: Label,
  domain: Type
}

export type NeutralTerm = {
  case: "neutral",
  applicant: Label,
  args: Term[]
}

export type HoleTerm = {
  case: "hole",
  holeId: Symbol,
  weakening: Label[],
  substitution: Substitution<Label, Term>
}

// Label

export type Label = {case: "label", value: string}

// Fresh

export function freshLabel(): Label {
  return {case: "label", value: ""}
}

export function freshHoleTerm(): HoleTerm {
  const holeId: unique symbol = Symbol()
  return {
    case: "hole",
    holeId,
    weakening: [],
    substitution: []
  }
}

export function freshHoleType(): HoleType {
  const holeId: unique symbol = Symbol()
  return {
    case: "hole",
    holeId
  }
}

// Creates an empty block (0 bindings) with a fresh hole
export function freshBlock(): Block {
  return {
    case: "block",
    bindings: [],
    body: freshHoleTerm()
  }
}

export function freshParameter(domain: Type): Parameter {
  return {
    case: "parameter",
    label: freshLabel(),
    domain
  }
}

// Format

// export type Format<A> = { [P in keyof A]: A extends Label ? Format<P> : P } & ts.ESMap<string, boolean>

export type FormatData = {
  indented?: boolean,
  unannotated?: boolean
}

export type Format<S extends Syntax> =
  {
    [Key in keyof S]:
      S extends Label ? S :
      S[Key] extends (infer T)[] ? (T extends Syntax ? Format<T>[] : T[]) :
      S[Key] extends Syntax ? Format<S[Key]> : S[Key]
  } 
  & FormatData

export function defaultFormat<S extends Syntax>(syntax: S): Format<S> {
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

export function lookupAt<S extends Syntax, T extends Syntax>(source: S, index: Index, gamma: Context): {target: T, gamma: Context} {
  throw new Error("unimplemented")
}

export function replaceAt<S extends Syntax, T extends Syntax>(source: S, index: Index, gamma: Context, replace: (target: T, gamma: Context) => T): S {
  throw new Error("unimplemented")
}

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
      | {case: "codomain", index: DataTypeIndex | HereIndex}
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
  