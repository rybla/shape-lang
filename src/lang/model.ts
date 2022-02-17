import { ModuleIndex, Index, Module, Term, Type, TypeIndex } from "./syntax";

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
export function fillHole(module: Module, index: Index, term: Term): Module {throw new Error()}

// Dig a hole at `index`.
export function digHole(module: Module, index: Index): Module {throw new Error()}

