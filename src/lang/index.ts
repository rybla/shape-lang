export type Index = {}

// import { Map } from "immutable"
// import { ArrowType, Binding, Block, Constructor, Context, DataType, Definition, LambdaTerm, Name, Parameter, Reference, Syntax, Term, Type } from "./syntax"

// export function lookupAt<S extends Syntax, T extends Syntax>(source: S, index: Index<S>, gamma: Context): {target: T, gamma: Context} {
//   throw new Error("unimplemented")
// }

// export function replaceAt<S extends Syntax, T extends Syntax>(source: S, index: Index<S>, gamma: Context, replace: (target: T, gamma: Context) => T): S {
//   throw new Error("unimplemented")
// }

// export type HereIndex = {readonly case: "here"}
// export const here: HereIndex = {case: "here"}

// export type Indexable = IndexStepable | IndexTerminal

// export type IndexStepable = 
//   | Block
//   | Definition
//   | Constructor
//   | Type
//   | Term
//   | Parameter
// export type IndexTerminal =
//   | Binding
//   | Name
//   | Reference

// type IndexStep_<S extends IndexStepable, P extends keyof S> = 
//   S[P] extends (infer Q)[] ? (Q extends Indexable ? [P, number, Index<Q>] : never) :
//   S[P] extends Indexable ? [P, Index<S[P]>] :
//   never
// type IndexStep<S extends IndexStepable> = 
//   <R>(run: <P extends keyof S>(_: IndexStep_<S, P>) => R) => R

// const indexStep =
//   <S extends IndexStepable>() =>
//   <P extends keyof S>(indexStep_: IndexStep_<S, P>): IndexStep<S> =>
//   (run) =>
//   run(indexStep_)

// export type Index<S extends Indexable> =
//   | HereIndex
//   // @ts-ignore
//   | S extends IndexStepable ? IndexStep<S> : HereIndex
  
// // ! doesn't work...
// // let i: Index<DataType> =
// //     indexStep<DataType>()<"reference">(["reference", here])
// // let j: Index<ArrowType> =
// //   indexStep<ArrowType>()<"parameters">(["parameters", 0, 
// //   indexStep<Parameter>()<"name">(["name", here])])

//   // | (S extends Reference ? HereIndex :
//   //   <P extends keyof S>(p: P) =>
//   //     S[P] extends (infer Q)[] ? (Q extends Syntax ? [number, Index<Q>] : never) :
//   //     S[P] extends Syntax ? Index<S[P]> : never)

// // let x: Reference = undefined as unknown as Reference
// // let _ = lookupAt<LambdaTerm, LambdaTerm>(
// //   {case: "lambda", body: {case: "block", definitions: [], body: {case: "neutral", reference: x, args: []}}},
// //   undefined as unknown as Index<LambdaTerm>,
// //   Map()
// // )

// // const i: Index<DataType> = 
// //   (): ["reference", Index<DataType["reference"]>] =>
// //   ["reference", here]

// // const j = i


// // (_:"body"): Index<LambdaTerm["body"]> => undefined as unknown as Index<LambdaTerm["body"]>

// // lookupAt<Module, Term>(undefined as unknown as Module, () => ({key: "statements", index: () => ({key: ""})}))

// // type IsVariant<A> = A extends (keyof (infer B)) ? "true" : "false"
// // function f<A, B>(x: IsVariant<A | B>): void {}
// // f()

//   // export type IndexStep<S extends Syntax, P extends keyof S> =
//   //   [P, ]

//     // {
//     //   [Key in keyof S]:
//     //     S[Key] extends (infer T)[] ? unknown :
//     //     S[Key] extends Syntax ? Index<S> :
//     //     never
//     // }

//   // S extends Label ? HereIndex :
//   // S extends Module ? 

// // export type Index<S extends Syntax> = 
// //   {
// //     [Key in keyof S]:
// //       S extends Label ? HereIndex :
// //       S[Key] extends (infer T)[] ? (T extends Syntax ? {i: number, index: Index<T>} : unknown) :
// //       S[Key] extends Module ? S[Key] : // TODO
// //       S[Key] extends Statement ? S[Key] : // TODO
// //       S[Key] extends Constructor ? S[Key] : // TODO
// //       S[Key] extends Type ? S[Key] : // TODO
// //       S[Key] extends Block ? S[Key] : // TODO
// //       S[Key] extends Binding ? S[Key] : // TODO
// //       S[Key] extends Term ? S[Key] : // TODO
// //       S[Key] extends Parameter ? S[Key] : // TODO
// //       unknown
// //   }

// // export type Index = ModuleIndex | StatementIndex | ConstructorIndex | TypeIndex | TermIndex | BlockIndex | BindingIndex | ParameterIndex

// // export type ModuleIndex = {}
// // export type StatementIndex = {}
// // export type ConstructorIndex = {}
// // export type TypeIndex = {}
// // export type TermIndex = {}
// // export type BlockIndex = {}
// // export type BindingIndex = {}
// // export type ParameterIndex = {}

// // // ModuleIndex

// // export type ModuleIndex = IndexStep<{case: "module", i: number, index: StatementIndex}>

// // // StatementIndex

// // export type StatementIndex = IndexStep<TermDefinitionIndex | DataDefinitionIndex>
// // export type TermDefinitionIndex = 
// //   {
// //     case: "term definition",
// //     sub:
// //       | {case: "label"}
// //       | {case: "type", index: TypeIndex}
// //       | {case: "term", index: TermIndex}
// //   }
// // export type DataDefinitionIndex =
// //   {
// //     case: "data definition",
// //     sub:
// //       | {case: "label"}
// //       | {case: "constructor", i: number, index: ConstructorIndex}
// //   }

// // // ConstructorIndex

// // export type ConstructorIndex =
// //   IndexStep<{
// //     case: "constructor", 
// //     sub: ConstructorSubIndex
// //   }>
// // export type ConstructorSubIndex =
// //   | {case: "label"}
// //   | {case: "domain", i: number, index: TypeIndex}

// // // TypeIndex

// // export type TypeIndex = IndexStep<ArrowTypeIndex | DataTypeIndex>
// // export type ArrowTypeIndex =
// //   {
// //     case: "arrow",
// //     sub:
// //       | {case: "domain", i: number, index: TypeIndex} 
// //       | {case: "codomain", index: DataTypeIndex | HereIndex}
// //   }
// // export type DataTypeIndex =
// //   {
// //     case: "data",
// //     sub:
// //       | {case: "label"}
// //   }

// // // BlockIndex

// // export type BlockIndex =
// //   IndexStep<{
// //     case: "block",
// //     sub:
// //       | {case: "binding", i: number, index: BindingIndex}
// //       | {case: "body", index: TermIndex}
// //   }>

// // export type BindingIndex =
// //   IndexStep<{
// //     case: "binding",
// //     sub:
// //       | {case: "label"}
// //       | {case: "type", index: TypeIndex}
// //       | {case: "term", index: TermIndex}
// //   }>

// // // TermIndex

// // export type TermIndex = IndexStep<LambdaTermTermIndex | NeutralTermTermIndex>
// // export type LambdaTermTermIndex =
// //   {
// //   case: "lambda",
// //   sub:
// //     | {case: "parameter", i: number, index: ParameterIndex}
// //     | {case: "body", index: TermIndex}
// //   }
// // export type NeutralTermTermIndex =
// //   {
// //     case: "neutral",
// //     sub:
// //       | {case: "applicant"}
// //       | {case: "argument", i: number, index: TermIndex}
// //   }

// // export type ParameterIndex =
// //   IndexStep<{
// //     case: "parameter", 
// //     sub:
// //       | {case: "label"}
// //       | {case: "domain", index: TypeIndex}
// //   }>

// // export function pushIndex(moduleIndex: ModuleIndex, index: Index): ModuleIndex {throw new Error()}
  