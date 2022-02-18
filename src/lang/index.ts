// Indexing

import { Context, Syntax } from "./syntax"

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
  