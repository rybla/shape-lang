# Spec

This file is the current working version of our plan for what transformations the editor should have.
It does not explain how they should be implemented.
For example, if a transformation creates type problems, it should recursively cause more digs.
But this file just lists what the transformations are.

- definition list manipulations
    - add a new definition
    - remove a definition
    - reorder definitions by moving a single definition to a different location.
- filling a hole
    - select something in scope, or U, Pi, lam.
- applying a function around a value
    - user indicates an argument term v (perhaps by putting their cursor to the left of it?)
      and selects a function f to be applied from a list (perhaps including Pi)
      with functions with a matching argument type on top of the list.
    - v may already be in a hole or not.
    - After applying the function, v may directly fit in an argument or end up in a hole.
      Also, the whole term (f v) may end up in a hole if
       - v started in a hole, or if v didn't start in a hole but (f v) doesn't fit where v was. 
- Expanding/contracting argument lists at call site
    - irrelevant if in expanded eta form, but can add or delete argument at end.
      E.g. if f : A -> B -> C, where it is called could be (f a b) or just (f a) or just f
    - After doing this, place whole neutral form (f ...) into hole if it isn't already
      (because this transformation always changes the type).
- dig
- function argument manipulations
    - add an argument
    - remove an argument
    - move an argument to a different position