---
layout: post
title: "Down with Show! Part 3: A replacement for Show"
---

This is part three of three in a series in which I will argue that it is time
to consign the `Show` type class to the dustbin of history. In [the first
post][] I discussed some rules of thumb for what I think makes a good type
class, and in [the second post][] I discussed what I think the shortcomings
of the `Show` class are. In this post I will propose a replacement for `Show`.

I'm going to use PureScript for this post, but it should be fairly
straightforward to implement these ideas in Haskell too.

I'll start by recapping what I think the shortcomings of `Show` from the
perspective of a class for displaying values in the repl are:

1. Many types, such as `(->)` or `IORef`, don't have instances.
2. The output for large and complex values is difficult to read, especially in
   values which have lots of nesting, because we can't do things like
   pretty-printing or cutting off once we reach a certain depth.
3. The fact that we produce a `String` makes it tempting to use the class for
   other purposes.

Firstly, we might want to ask whether replacing `Show` with another class is
really the right thing to do, especially since this class will probably look
quite similar to `Show`, and so it will probably also fail to abide by the
rules of thumb which we saw previously. However, since this class will
explicitly only be for use in the repl or for debugging, i.e. since it should
never appear in production code, we can probably be a little more permissive.
Furthermore, a type class is undeniably useful for displaying values in the
repl; it's hard to imagine how we might replace the current type-class-based
design of "call this class's member function on the result to turn it into
something printable".

Since this class is going to be specifically for debugging purposes, we are
going to call it `Debug`. So far we've got something like this

```
class Debug a where
  debug :: a -> Repr
```

where `Repr` is a type which we haven't nailed down the details of just yet,
but we know that we want it to represent some sort of tree structure.

We'll first need to provide some functions for constructing values of the type
`Repr` from primitive types:

```
int :: Int -> Repr
number :: Number -> Repr
boolean :: Boolean -> Repr
char :: Char -> Repr
string :: String -> Repr
array :: Array Repr -> Repr
record :: Array (Tuple String Repr) -> Repr
```

The functions `int`, `number`, `boolean`, `char`, and `string` are for
constructing leaves in this tree, whereas the functions `array` and `record`
will usually have children, which they receive as arguments.

We'll also want a function for algebraic data types with named constructors:

```
constructor :: String -> Array Repr -> Repr
```

This would allow us to create a `Repr` out of e.g. `Just 3`, with the
expression `constructor "Just" [int 3]`.

This gets us most of the way there, but we still need to deal with types like
`Ref` or `(->)` which can't be given injective instances, as well as collection
types like `Map` which keep their internal structure hidden.

We will call types like `Ref` or `(->)` which can't be given an injective
`Show` instance "opaque". They should probably be handled separately, so we
will provide a separate function for them:

```
opaque :: String -> Array (Tuple String Repr) -> Repr
```

The arguments to `opaque` should be the name of the type and an association
list of any other information which might be useful to see in a debugging
representation. For instance, we could create a `Repr` for a function `a -> b`
with the expression `opaque "function" []`, or perhaps even

```
opaque "function"
  [ Tuple "domain" (typeRepr (Proxy :: Proxy a))
  , Tuple "codomain" (typeRepr (Proxy :: Proxy b))
  ]
```

if we are able to implement a function along the lines of

```
typeRepr :: forall a. Typeable a => Proxy a -> Repr
```

to produce a value-level representation of the type `a`.

Finally, to handle collection types like `Map` which keep their internal
structure hidden, we will add two more functions:

```
collection :: String -> Array Repr -> Repr
assoc :: String -> Array (Tuple Repr Repr) -> Repr
```

The `collection` function creates a `Repr` out of something conceptually
representing a sequence of values; this could be used for e.g. a hash-array
mapped trie type. The first argument should be the name of the type, and the
second should be the contents. The `assoc` function is similar: it's intended
for types such as `Map`, and the arguments again should be the name of the type
and the contents.

Remember that the aim here is that every single type of kind `Type` should have
a `Debug` instance. Therefore, we need to provide a mechanism for deriving
these instances (otherwise it's going to be too much of a pain). Thankfully
this is not too difficult; using `purescript-generics-rep`, we can provide a
`GenericDebug a` class, which allows us to write e.g.

```
instance debugMyType :: Debug MyType where
  debug = genericDebug
```

whenever all of the types appearing in the definition of `MyType` themselves
have `Debug` instances. What this means is that in most cases, users of this
library won't have to use the functions above, as they can just use existing
`Debug` instances. However, users who are defining opaque types or types with
hidden representations will need to use some of the above functions to define
their `Debug` instances by hand, since the instance provided by `genericDebug`
will not be what you want.

Now all we need is a function

```
prettyPrint :: Repr -> String
```

for displaying these values in the repl, together with a bunch of `Debug`
instances for all of the types in the core libraries. Let's put it all together
and see what we get:

```
module Test.Main2 where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Debug (class Debug, debug, genericDebug, prettyPrint)
import Data.Map (Map)
import Data.Tuple (Tuple(..), swap)
import Data.Map as Map
import Effect (Effect)
import Effect.Console (log)

data MyType a
  = A Int a
  | B (Int -> Int) (Array Int)
  | C (Map String a) (Map a String)

derive instance genericMyType :: Generic (MyType a) _

instance debugMyType :: Debug a => Debug (MyType a) where
  debug = genericDebug

main :: Effect Unit
main = do
  let
    p :: forall a. Debug a => a -> Effect Unit
    p = log <<< prettyPrint <<< debug

  p (A 3 false)
  p (A 3 (A 4 true))
  p (B (_ + 1) [10,15,3] :: MyType Void)

  let items = [Tuple "a" 3, Tuple "b" 6]
  p (C (Map.fromFoldable items) (Map.fromFoldable (map swap items)))
```

This outputs the following:

```
A 3 false
A 3 (A 4 true)
B <function> [ 10, 15, 3 ]
C
  <Map { "a": 3, "b": 6 }>
  <Map { 3: "a", 6: "b" }>
```

Notice that we use parentheses only where necessary &mdash; see the second
line in partiular &mdash; and also that that the `C` constructor, being more
complex, uses multiple lines for its pretty-printed representation.

We can even go further and define a more flexible pretty-printing function:

```
prettyPrintWith ::
  { maxDepth :: Maybe Int
  , compactThreshold :: Int
  } -> Repr -> String
```

to configure things like the maximum depth before cutting off, or how large a
tree may become before it is considered too large to appear on one line.

That's not all, though. Since the `Repr` data type retains the tree structure,
we can also define a type

```
data ReprDelta :: Type
```

which represents the *difference* between any two `Repr` values, together with
a function

```
diff :: Repr -> Repr -> ReprDelta
```

for diffing `Repr` values, plus another pretty-printing function

```
prettyPrintDelta :: ReprDelta -> String
```

One thing that is really nice about that is that it enables diffing arbitrary
values; all we need is a `Debug` instance! This is really useful for diffing
expected versus actual values in tests, for example. Using the above API, it's
easy to provide a function

```
assertEqual :: forall a. Eq a => Debug a => a -> a -> Effect Unit
```

which enables us to do this:

```
  let
    items = [Tuple "a" 3, Tuple "b" 6, Tuple "c" 13]
    x = Map.fromFoldable items
    y = Map.fromFoldable (items <> [Tuple "b" 9])
  in
    assertEqual x y
```

which produces:

<pre><code>Test failed:
<Map
{ "a": 3,
  "b": <span style="color:red">-6</span> <span style="color:green">+9</span>,
  "c": 13 }></code></pre>

with colours in your terminal.

Let's go back and check we've addressed all of the the shortcomings of `Show`
with this new design.

### Many types, such as `(->)` or `IORef`, don't have `Show` instances.

This should hopefully be solved with the `opaque` constructor. I haven't yet
come across a type which can't be given a useful `Debug` instance, but if
one day we do encounter such a type, it's easy to add a dedicated
constructor function for it without causing a breaking change to this API.

### The `Show` output for large and complex values is difficult to read.

This is solved by the `prettyPrint` and `prettyPrintWith` functions.

### The fact that we produce a `String` makes it tempting to abuse the `Show` class for other purposes.

Now, when we provide `Debug` instances, we are constrained in that we may
only produce a `Repr` value rather than any old `String`. We don't expose any
`Repr` constructors; the only way we can create these values is via the
functions I've described above. This constraint ensures that a `Debug` instance
is very unlikely to be useful in any situation other than its intended purpose.

## I want this!!!

Of course you do. You can check out [my PureScript implementation on GitHub](https://github.com/hdgarrood/purescript-debugged).
I probably won't do a Haskell port, so feel free to port it to Haskell
yourself.

[the first post]: {{ site.baseurl }}{% post_url 2018-12-12-down-with-show-part-1 %}
[the second post]: {{ site.baseurl }}{% post_url 2018-12-13-down-with-show-part-2 %}
