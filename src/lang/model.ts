import { Map } from "immutable";
import { ModuleIndex, Index, Module, Term, Type, TypeIndex, DataType, HoleType, Label, NeutralTerm, Block } from "./syntax";

type Change = {
  case : "input",
  change : Change,
  index : number
// } | {
  // case : "output"
} | {
  case : "deleteInput",
  index : number
} | {
  case : "insertInput",
  index : number
} | {
  case : "changeInput",
  index : number,
  newType : Type
} | {
  case : "changeOutput",
  newType : DataType | HoleType
}

type Either<a,b> = {
  case : "left",
  val : a
} | {
  case : "right",
  val : b
}
type Delete = {}
type ChangeCtx = Map<Label, Either<Change, Delete>>

function changeTerm(term : Term, changes : ChangeCtx, change : Change) : Term {
  if(term.case === "lambda"){
  } else if(term.case === "neutral"){
  } else if(term.case === "hole"){
  }
  throw new Error("shouldn't get here");
}
function changeNeutral(neu : NeutralTerm, changes : ChangeCtx, change : Change) : NeutralTerm {
  throw new Error();
}
function changeBlock(neu : Block, changes : ChangeCtx, change : Change) : Block {
  throw new Error();
}
function searchBlock(neu : Block, changes : ChangeCtx) : Block {
  throw new Error();
}

// Remove the statement at `index`.
export function removeStatement(module: Module, index: ModuleIndex): Module {throw new Error()}

// Remove the constructor at `index`.
export function removeConstructor(module: Module, index: ModuleIndex): Module {throw new Error()}

// Remove the binding at `index`.
export function removeBinding(module: Module, index: ModuleIndex): Module {throw new Error()}

// Insert `domain` at `domainIndex` relative to the top of the type of the
// binding at `parentIndex`.
export function insertDomain(
  module: Module,
  parentIndex: ModuleIndex,
  domainIndex: TypeIndex,
  domain: Type
): Module {throw new Error()}

// Remove the domain at `domainIndex` relative to the top of the type of the
// binding at `parentIndex`.
export function removeDomain(
  module: Module,
  parentIndex: ModuleIndex,
  domainIndex: TypeIndex
): Module {throw new Error()}

// Move the domain at `sourceDomainIndex` to `targetDomainIndex`, which are
// domains of the arrow type at `arrowIndex`, relative to the top of the type of
// the binding at `parentIndex`.
export function moveDomain(
  module: Module,
  parentIndex: ModuleIndex,
  arrowIndex: TypeIndex,
  sourceDomainIndex: number,
  targetDomainIndex: number
): Module {throw new Error()}

// Fill the hole at `index` with `term`
export function fillHoleTerm(module: Module, index: Index, term: Term): Module {throw new Error()}

// Dig a hole at `index`.
export function digHoleTerm(module: Module, index: Index): Module {throw new Error()}

