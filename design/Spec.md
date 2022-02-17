# Spec

This file is the current working version of our plan for what transformations the editor should have.
It does not explain how they should be implemented.
For example, if a transformation creates type problems, it may recursively cause more digs.
But this short file just lists what the transformations are.

- definition list manipulations
    - add a new definition
    - remove a definition
    - reorder definitions by moving a single definition to a different location.
- Some sort of way to work on function applications within a hole
    - Apply a function around a term in the program?
    - Have a scratch area in which terms can be dragged around?
    - Etc...
- dig
- function argument manipulations
    - add an argument
    - remove an argument
    - move an argument to a different position
    - change output type
