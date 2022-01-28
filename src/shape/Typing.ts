import { List } from "immutable";
import { Context, Hole, Term } from "./Grammar";

// Checks that `a` has type `alpha`.
export function check(context: Context, a: Term, alpha: Term): boolean {
  throw new Error("unimplemented");
}

// Infers the type of `a`.
export function infer(context: Context, a: Term): Term | undefined {
  throw new Error("unimplemented");
}

export type Substitution = List<[Hole, Term]>

// Unifies the terms `a1` and `a2`.
export function unify(context: Context, a1: Term, a2: Term): Substitution | undefined {
  throw new Error("unimplemented");
}
