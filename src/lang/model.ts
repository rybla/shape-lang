import { BindingIndex, Index, Module, Term, Type, TypeIndex } from "./syntax";

// Delete the statement at `index`.
export function deleteStatement(module: Module, index: Index): Module {throw new Error()}

// Delete the constructor at `index`.
export function deleteConstructor(module: Module, index: Index): Module {throw new Error()}

// Delete the binding at `index`.
export function deleteBinding(module: Module, index: Index): Module {throw new Error()}

// Insert `domain` at `domainIndex` in the type of the name bound at `bindingIndex`.
// Note that `domainIndex` is relative to the top of the type of the binding at `bindingIndex`.
export function insertDomain(module: Module, domain: Type, domainIndex: TypeIndex, bindingIndex: BindingIndex): Module {throw new Error()}

// Delete the domain at `domainIndex` in the type of the name bound at `bindingIndex`.
// Note that `domainIndex` is relative to the top of the type of the binding at `bindingIndex`.
export function deleteDomain(module: Module, domainIndex: TypeIndex, bindingIndex: BindingIndex): Module {throw new Error()}

// Move the domain at `sourceDomainIndex` to `targetDomainIndex`.
// Note that `domainIndex` is relative to the top of the type of the binding at `bindingIndex`.
export function moveDomain(module: Module, sourceDomainIndex: TypeIndex, targetDomainIndex: TypeIndex, bindingIndex: BindingIndex): Module {throw new Error()}

// Fill the hole at `index` with `term`
export function fillHole(module: Module, index: Index, term: Term): Module {throw new Error()}

// Dig a hole at `index`.
export function digHole(module: Module, index: Index): Module {throw new Error()}

