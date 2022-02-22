import { ArrowType, Block, Case, Constructor, DataDefinition, DataType, Definition, Label, LambdaTerm, MatchTerm, Name, NeutralTerm, Parameter, Syntax, Term, TermBinding, TermDefinition, TermReference, Type, TypeBinding, TypeReference, UniqueTermBinding } from "./syntax";

export type IndexStepable = 
  | Block
  | Definition
  | Constructor
  | Type
  | Term
  | Case
  | Parameter
export type IndexTerminal =
  | Label
  | TermBinding
  | UniqueTermBinding
  | TypeBinding
  | TermReference
  | TypeReference

export type IndexHere = {case: "here"}
export const here: IndexHere = {case: "here"}

export type IndexStep<S extends Syntax, Key extends keyof S> =
  S extends IndexTerminal ? IndexHere :
  S extends IndexStepable ? (
    S[Key] extends (infer T)[] ? (T extends Syntax ? {case: Key, i: number, index: Index<T>} : never) :
    S[Key] extends infer T ? (T extends Syntax ? {case: Key, index: Index<T>} : never) :
    never) : 
  never

// If only I could write generative sum types...
export type Index<S extends Syntax> = 
  | IndexHere
  | ( // Block
      S extends Block ? 
        ( IndexStep<Block, "definitions">
        | IndexStep<Block, "body"> ) :
      S extends Definition ?
        ( IndexStep<TermDefinition, "uniqueTermBinding"> 
        | IndexStep<TermDefinition, "type"> 
        | IndexStep<TermDefinition, "term">
        | IndexStep<DataDefinition, "typeBinding">
        | IndexStep<DataDefinition, "constructors"> ) :
      S extends Constructor ?
        ( IndexStep<Constructor, "uniqueTermBinding"> 
        | IndexStep<Constructor, "parameters"> ) :
      S extends Type ?
        ( IndexStep<ArrowType, "parameters">
        | IndexStep<ArrowType, "output"> 
        | IndexStep<DataType, "typeReference"> ) :
      S extends Term ?
        // LambdaTerm
        ( IndexStep<LambdaTerm, "termBindings">
        | IndexStep<LambdaTerm, "block">
        // NeutralTerm
        | IndexStep<NeutralTerm, "termReference">
        | IndexStep<NeutralTerm, "args"> 
        // MatchTerm
        | IndexStep<MatchTerm, "typeReference">
        | IndexStep<MatchTerm, "term">
        | IndexStep<MatchTerm, "cases"> ) :
      S extends Case ?
        ( IndexStep<Case, "termBindings">
        | IndexStep<Case, "termReference">
        | IndexStep<Case, "block"> ) :
      S extends Parameter ?
        ( IndexStep<Parameter, "label">
        | IndexStep<Parameter, "type"> ) :
      S extends UniqueTermBinding ? IndexHere :
      S extends TermBinding ? IndexHere :
      S extends TypeBinding ? IndexHere :
      S extends TermReference ? IndexHere :
      S extends TypeReference ? IndexHere :
      S extends Name ? IndexHere :
      never
    )
    
export function concatIndex<S1 extends IndexStepable, S2 extends IndexStepable>(index1: Index<S1>, index2: Index<S2>): Index<S1> {
  // throw new Error("umimplemented: concatIndex")
  switch (index1.case) {
    case "definitions":
    case "type":
    case "term":
    case "constructors":
    case "parameters":
    case "output":
    case "args":
    case "cases":
    case "block":
    case "body":
      return {...index1, index: concatIndex(index1.index, index2)}
    case "here":
      return index2 as Index<S1>
    default: throw new Error("could not concatIndex for " + index1.case)
  }
}

// export function pushIndexStep<S1 extends IndexStepable, S2 extends IndexStepable>(index: Index<S1>, step: Index<S2>["case"]): Index<S1> {
//   switch (index.case) {
//     case "definitions":
//     case "type":
//     case "term":
//     case "constructors":
//     case "parameters":
//     case "output":
//     case "args":
//     case "cases":
//     case "block":
//     case "body":
//       return {...index, index: pushIndexStep(index.index, step)}
//     case "here":
//       return {case: step as Index<S2>["case"], index: here}
//   }
//   throw new Error("could not pushIndexStep for " + index.case)
// }