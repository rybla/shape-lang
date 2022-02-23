# Spec

This file is the current working version of our plan for what transformations the editor should have.
It does not explain how they should be implemented.
For example, if a transformation creates type problems, it may recursively cause more digs.
But this short file just lists what the transformations are.

- definition list manipulations
    - add a new definition
    - remove a definition
    - reorder definitions by moving a single definition to a different location.
- Neutral form building
    - Holes have a set of neutral forms in them
    - Add new neutral form by selecting a variable in context
        - Arguments are holes
    - Place one neutral form in hole of another one
- dig
- Type declaration manipulations
    - add an argument
    - remove an argument
    - move an argument to a different position
    - change output type
