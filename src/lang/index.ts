import { ArrowType, Binding, Block, Case, Constructor, DataDefinition, DataType, Definition, HoleTerm, HoleType, LambdaTerm, MatchTerm, Name, NeutralTerm, Parameter, Reference, Syntax, Term, TermDefinition, Type, UniqueBinding } from "./syntax";

type IndexStepable = 
  | Block
  | Definition
  | Constructor
  | Type
  | Term
  | Case
  | Parameter
type IndexTerminal =
  | Binding
  | UniqueBinding
  | Name
  | Reference

export type IndexHere = {case: "here"}

export type IndexStep<S extends Syntax, Key extends keyof S> =
  S extends IndexTerminal ? IndexHere :
  S extends IndexStepable ? (
    S[Key] extends (infer T)[] ? (T extends Syntax ? {case: Key, i: number, index: Index<T>} : never) :
    S[Key] extends infer T ? (T extends Syntax ? {case: Key, index: Index<T>} : never) :
    never) : 
  never

// If only I could write generative sum types...
export type Index<S extends Syntax> = 
  | {case: "here"}
  | // Block
    S extends Block ? 
      ( IndexStep<Block, "definitions">
      | IndexStep<Block, "body"> ) :
    // Definition
    S extends TermDefinition ?
      ( IndexStep<TermDefinition, "uniqueBinding"> 
      | IndexStep<TermDefinition, "type"> 
      | IndexStep<TermDefinition, "term"> ) :
    S extends DataDefinition ?
      ( IndexStep<DataDefinition, "id">
      | IndexStep<DataDefinition, "constructors"> ) :
    S extends Constructor ?
      ( IndexStep<Constructor, "uniqueBinding"> 
      | IndexStep<Constructor, "parameters"> ) :
    // Type
    S extends ArrowType ?
      ( IndexStep<ArrowType, "parameters">
      | IndexStep<ArrowType, "output"> ) :
    S extends DataType ? IndexHere :
    S extends HoleType ? IndexHere :
    // Term
    S extends LambdaTerm ?
      ( IndexStep<LambdaTerm, "ids">
      | IndexStep<LambdaTerm, "block"> ) :
    S extends NeutralTerm ?
      ( IndexStep<NeutralTerm, "reference">
      | IndexStep<NeutralTerm, "args"> ) :
    S extends MatchTerm ?
      ( IndexStep<MatchTerm, "reference">
      | IndexStep<MatchTerm, "term">
      | IndexStep<MatchTerm, "cases"> ) :
    S extends HoleTerm ? IndexHere :
    S extends Case ?
      ( IndexStep<Case, "bindings">
      | IndexStep<Case, "block"> ) :
    S extends Parameter ?
      ( IndexStep<Parameter, "name">
      | IndexStep<Parameter, "type"> ) :
    S extends UniqueBinding ? IndexHere :
    S extends Binding ? IndexHere :
    S extends Reference ? IndexHere :
    S extends Name ? IndexHere :
    never
    
export function concatIndex<S1 extends Syntax, S2 extends Syntax>(i1: Index<S1>, i2: Index<S2>): Index<S1> {throw new Error()}
