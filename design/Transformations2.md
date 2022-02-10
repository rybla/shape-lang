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
<term> ::= λ [<name> : <type>] . <prog> | <type>
<type> ::= Π [<name> : <type>] . <prog>
```
