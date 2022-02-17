import { BindingIndex, Index, Module, Term, Type, TypeIndex } from "./syntax";

// Remove the statement at `index`.
export function removeStatement(module: Module, index: Index): Module {throw new Error()}

// Remove the constructor at `index`.
export function removeConstructor(module: Module, index: Index): Module {throw new Error()}

// Remove the binding at `index`.
export function removeBinding(module: Module, index: Index): Module {throw new Error()}

// Insert `domain` at `domainIndex` in the type of the name bound at `bindingIndex`.
// Note that `domainIndex` is relative to the top of the type of the binding at `bindingIndex`.
export function insertDomain(module: Module, bindingIndex: BindingIndex, domain: Type, domainIndex: TypeIndex): Module {throw new Error()}

// Remove the domain at `domainIndex` in the type of the name bound at `bindingIndex`.
// Note that `domainIndex` is relative to the top of the type of the binding at `bindingIndex`.
export function removeDomain(module: Module, bindingIndex: BindingIndex, domainIndex: TypeIndex): Module {throw new Error()}

// Move the domain at `sourceDomainIndex` to `targetDomainIndex`.
// Note that `domainIndex` is relative to the top of the type of the binding at `bindingIndex`.
export function moveDomain(module: Module, bindingIndex: BindingIndex, sourceDomainIndex: TypeIndex, targetDomainIndex: TypeIndex): Module {throw new Error()}

// Fill the hole at `index` with `term`
export function fillHole(module: Module, index: Index, term: Term): Module {throw new Error()}

// Dig a hole at `index`.
export function digHole(module: Module, index: Index): Module {throw new Error()}

