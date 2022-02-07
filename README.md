# shape-lang

A simple dependently-typed programming language based on holes and
metaprogramming.

## TODO

Henry

- [x] editor transformations
  - `shape/Transformation.ts`
- [ ] GUI editor
  - how to navigate AST? arrow keys?

Jacob

- [ ] normalization
- [ ] type-checking
- [ ] type-inference
- [ ] unification

NOTE on how to develop with typescript: First, follow the installation from
https://github.com/TypeStrong/ts-node

which is npm install -g typescript npm install -g ts-node

Next, go into 'tsconfig.json' and replace "module": "esnext", with "module":
"commonjs",

When you run webpack, it will switch it back automatically (if I remember
correctly?)

Then, run ts-node. You will then be able to import files using the standard
typescript import syntax from the repl.
