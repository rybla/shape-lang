# Transformations

## Syntax

New features:

- Parameters/arguments for lambdas, pis, and applications are now represented as
  lists
  - This allows easier manipulation of parameters/arguments
- Holes cannot appear as the applicant of an application
  - It doesn't make sense to give the arguments to a function before you know
    what the function is.
- Lambdas cannot appear as the applicant of an application
  - This never arises in practice, and just makes dealing with arguments
    annoying.
- Let-bindings cannot be nested.
  - This never arises in practice, since you can always just un-nest a nested
    let to achieve the exact same result (in terms of which variables are in
    context at which points).

```
<prog> ::= let [<name> : <type> = <term>] in <term>
<term> ::= λ [<name> : <type>] . <prog> | <neu> | <type> | <hole>
<neu>  ::= <var> [<term>]
<type> ::= Π [<name> : <type>] . <prog>
```

## Transformations on let-bindings

_Add parameter_

```
let f .. : _ = _ in _[f ..] ~>
let f .. (x:?) .. : _ = _ in _[f .. ? ..]
```

_Remove parameter_

```
let f .. (x:A) .. : _ = _ in _[f .. a ..] ~>
let f .. : _ = _ in _[f ..]
```

_Rearrange parameters parameter_

```
let f .. (x:A) .. (y:B) .. : _ = _ in _[f .. a .. b ..] ~>
let f .. (y:B) .. (x:A) .. : _ = _ in _[f .. b .. a ..]
```

_Add deep parameter_

TODO: write algorithm to demonstrate this.

_Dig type of parameter_

```
let f .. (x:A) .. : _ = _ in _[f ..  a  ..] ~>
let f .. (x:?) .. : _ = _ in _[f .. {a} ..]
```

_Dig type of output_

```
let f .. : A = _ in _[ f .. ] ~>
let f .. : ? = _ in _[{f ..}]
```
