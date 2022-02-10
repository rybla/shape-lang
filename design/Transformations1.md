# Transformations

The goal of this file is to list transformations that we want the editor to be
able to do. We should develop the theory behind it with these in mind. Or
determine that that there is no nice theory behind it.

No need to list obvious ones like "Fill a hole" or "Dig" or "Use a variable"

## General Thoughts

Henry

- I'm a little skeptical of making any use of "holes with content" i.e. when a
  user's code is replaced with a hole, the original code can be put in the hole
  like some sort of buffer.
  - I can see that you come up with a few use cases where the content of the
    hole is simply a let-bound name that had its type changed or whatever, so
    perhaps that is acceptable. But its still a discontinutity from how the hole
    logic works in general.
  - The more principled solution is to just use buffers for everything, which
    ends up kinda like gradual typing as we talked about before. In this case,
    the inside of a hole is a buffer that hasn't been typechecked with the
    expected type for the hole yet. The hole defines a "boundary" between two
    internally-coherent programs that have not been checked to fit together.
    This can be pretty useful as a way to handle when transformations break
    typing in a way that they need to later resolve, as you've shown with the
    more limited ability (just using let-bound names) I mentioned above. But
    this leads to a very complicated world with holes inside holes inside whole,
    since a hole defines a buffer that contains an arbitrary program.
- There are two kinds of non-local changes that occur from transformations:
  changes that result from hole unifications, and changes that result from
  modifying the value/type of a let-bound name somehow. The way transformations
  interact with let-bindings is special from the rest of the syntax, and perhaps
  this indicates that let-bindings is actually a metaprogrammatic rather than
  syntactic structure.
  - This makes intuitive sense to me because, conceptually, I think of a
    let-binding as just an abbreviation for using the same value in multiple
    places (sounds metaprogrammatic), not as calling a function as the
    lambda-on-the-left interpretation implies
  - So, if we think of let-binding as a metaprogram, then perhaps there's a way
    of encoding the transformations we want into the behavior of this
    let-binding metaprogram rather than try to reinvent traversals over the
    syntax that let-binding already does.

### Let-related transformations

_Add parameter_

```
let f ..          : _ = _ in _[f ..] ~>
let f .. (x:A) .. : _ = _ in _[f .. ? ..]
```

_Remove parameter_

```
let f .. (x:A) .. : _ = _ in _[f .. a ..] ~>
let f ..          : _ = _ in _[f ..]
```

_Retype parameter_

```
let f .. (x:A) .. : _ = _ in _[f ..  a  ..] ~>
let f .. (x:B) .. : _ = _ in _[f .. {a} ..]
```

_Retype output_

```
let f .. : A = _ in _[f ..] ~>
let f .. : B = _ in _[{f ..}] ~>
```

_Rearrange parameters_

```
let f .. (x:A) .. (y:B) .. : _ = _ in _[f .. a .. b ..] ~>
let f .. (y:B) .. (x:A) .. : _ = _ in _[f .. b .. a ..]
```

### Application-related transformations

TODO

## Add an argument to an existing function

_STATUS:_ definitely want

```
FROM

let f x y = ...
in ... (f a b) ...

TO

let f x z y = ...
in ... (f a ? b)
```

## Add, remove, or swap an argument to an application which has a hole on the left

_STATUS:_ probably want

```
FROM
? a b c
TO
? a b d c
OR
? a c
```

Example of why you would want this:

```
START
let f : A -> C
let g : A -> B -> C
in f a
NEXT dig f
let f : A -> C
let g : A -> B -> C
in ? a
NEXT fill g in hole
let f : A -> C
let g : A -> B -> C
in {g} a
NEXT add b to argument list
let f : A -> C
let g : A -> B -> C
in {g} a b
FINALLY now that the type matches fill hole with contents
let f : A -> C
let g : A -> B -> C
in g a b
```

On the other hand, the reason for not wanting this transformation is that having
arguments doesn't make sense without knowing what function that the are
arguments for. So maybe instead of applications, we just have functions which
take arguments as holes as a construct. C syntax makes this view of applications
more intuitive: (? a b) looks like it makes sense but ?(a,b) not so much.

- [henry] Not sure what this means. you mean that there is a special syntactic
  construct for "an application with a hole as its applicant"?

[henry] I lean towards not allowing holes on the left of applications at all.
because then there can be as many or few arguments as you want of any type, so
its basically just an unstructured list of terms where the editor can't really
help you at all. I can't think of a case when you'd actually want this. In the
example you gave, I think the proper way to do it would be to first copy `a`,
then replace `f a` with `g ? ?`, then paste `a` in the first hole and contstruct
`b` in the second hole.

[henry] there's probably a case to be made for "swapping" a function for another
function of a compatible type i.e.

```
let f : A -> A -> B = ..
let g : A -> A -> B = ..
f a1 a2

~>

let f : A -> A -> B = ..
let g : A -> A -> B = ..
g a1 a2
```

## Change the type of a function in a hole on the right

_STATUS:_ probably want

```
FROM
let f : (A -> B) -> C
let g : A -> B
in ... (f g) ...

TO
let f : (A -> B) -> C
let g : A -> C -> B
in ... (f {g}) ...
```

anywhere that a function no longer fits on the right, it should go into a hole.

[henry] this is already solved by the transformations that modify the type of a
let if we require applications to always be eta-expanded (even if they are not
displayed this way).

## Modify one function to accomadate another

_STATUS:_ don't want

```
FROM

f : (A -> B) -> C
f = ?
g : A -> B
... (f g) ...

NEXT - user digs "A" in f's signature

f : (? -> B) -> C
f = ?
g : ? -> B
... (f g) ...
```

So g gets modified as a result of changing f's argument. This would be
counterintuitive to a user as it creates an unusual nonlocal change.

[henry] I didn't think about this before, but yeah if you dig a hole that is
determined by unification, then nothing will happen.

Instead,

## Change an argument

_STATUS:_ probably want

```
FROM

f : (A -> B) -> C
f = ?
g : A -> B
... (f g) ...

NEXT - user digs "A" in f's signature

f : (? -> B) -> C
f = ?
g : A -> B
... (f {g}) ...
```

If an argument no longer fits, put it in a hole so that the user remembers that
it was there, but the code still typechecks.

[henry] The insight here is the mirror-like relationship between filling and
digging:

- When typechecking after a fill, a unification between a hole and a term
  produces a substitution that also fills the hole with the term.
- When typechecking after a dig, a unification between a hole and a term
  produces a replacement that also digs the term.

## Modify an in place lambda as a result of an argument change

_STATUS:_ probably don't want

```
FROM

let f : (A -> C) -> D = ?
in f (lam a . stuff)

TO - user adds argument to argument of f

let f : (A -> B -> C) -> D = ?
in f (lam a b . stuff)
```

The alternative is to use the previous rule instead, so we would get the lambda
put into a hole.

```
let f : (A -> B -> C) -> D = ?
in f {lam a . stuff}

NEXT - user adds argument to lambda

let f : (A -> B -> C) -> D = ?
in f {lam a b . stuff}

FINALLY - user fills hole

let f : (A -> B -> C) -> D = ?
in f (lam a b . stuff)
```

Argument against: unexpected nonlocal change

Argument for: lambdas on the r.h.s. of an application are anonymous, and only
exist as an argument to that particular function. Therefore, it makes sense that
they would follow whichever changes that the function undergoes. Contrast this
with the previous transformation, in which a named function which may be used
elsewhere was the argument.

## Add an argument to a let-bound function, except that it actually is a lambda and an application and not a let

_STATUS:_ unsure

```
START

(lam f : A -> C . (f a)) (lam a . stuff)

NEXT - add an argument

(lam f : A -> B -> C . (f a ?)) (lam a b . stuff)
```

My point is that this is equivalent to adding an argument to a let bound
function. If a user DID use a lambda on the left of an application and wanted to
do this transformation, the how else would they do it? On the other hand, why
would a user want a lambda to be on the left of an application?

Here is how you would do it without this mechanism:

```
(lam f : A -> C . (f a)) (lam a . stuff)
(lam f : A -> C . (f a)) {lam a . stuff}        -- put argument in hole
(lam f : A -> B -> C . (f a)) {lam a . stuff}   -- add arg to f
(lam f : A -> B -> C . (f a)) {lam a b . stuff} -- add arg to right lambda
(lam f : A -> B -> C . (f a)) (lam a b . stuff) -- fill hole
```

Note that step 3 would clearly be allowed as the type of the right lambda
expression is unconstrained because it is in a hole. On the other hand, it is
less clear what rule would allow step 2, because the left lambda's type is
constrained as it is applied to an argument (even though the argument is a
hole). Lambdas on the left of applications seem to cause problems in general.

So, for any given scheme for transformations, it should do one of the
following: - Don't allow lambdas on left of application - Allow them but accept
that they are unusable and no one will use them anyway - Treat lambdas on left
as as let expressions as far as the rules are concerned. - Allow step 2) above
as a result of the r.h.s. being a hole. - ....

## Changing input to an argument to a let bound lambda

_STATUS:_ depends on how lets work

Change input to input:

```
FROM
let f : (A -> C) -> D
let f = lam g . ... (g a) ...

TO
let f : (A -> B -> C) -> D
let f = lam g . ... (g a ?) ...

OR
let f : (A -> B -> C) -> D
let f = lam g . ... ({g} a) ...
```

^this second one contrasts the idea that we shouldn't end up with holes on the
left.

If we pick the first of these, then there seems to be something with +/-
positioning of argument types. For example, by contrast:

## Changing output to an argument to a let bound lambda

_STATUS:_ depends on how lets work

```
FROM
let f : (A -> B) -> D
let f = lam g . ... (g a) ...

TO
let f : (A -> C) -> D
let f = lam g . ... {g a} ...

OR
let f : (A -> C) -> D
let f = lam g . ... {g} a ...
```

We need to put g in a hole, but do we put it's arguments in the hole with it?

So chaning things in - position causes argument lists to change somewhere, while
changing something in + position could cause an entire application of variable
to argument to go into a hole (if we decide on those options.)

## Putting a function around a value (which may be in a hole)

_STATUS:_ probably want

```
FROM
let f : A -> B
a

TO
let f : A -> B
(f a)
```

```
FROM
let f : A -> B
c

TO
let f : A -> B
(f {c})
```

```
FROM
let f : A -> B
let g : B -> C
g {a}

TO
let f : A -> B
let g : B -> C
g (f a)
```

```
FROM
let f : A -> B
let g : B -> C
g {d}

TO
let f : A -> B
let g : B -> C
g (f {d})
```

```
FROM
let f : A -> B
let g : C -> D
g {a}

TO
let f : A -> B
let g : B -> C
g {f a}
```

These are useful for something like

```
FROM
list : List Bool
NEXT
filter ? list
NEXT
sum {filter ? list}
NEXT
sum {map ? (filter ? list)}
NEXT
sum {map toInt (filter ? list)}
NEXT
sum (map toInt (filter ? list))
```

building up a computation from inside out is pretty common when programming.
