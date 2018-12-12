---
layout: post
title: "Down with Show! Part 1: Rules of thumb for when to use a type class"
---

> I find it enlightening that in functional programming, usually the best
> solution is to program with functions. ;)
>
> &mdash; *Edward Kmett, from the /r/haskell thread [Definitive guide on when to use typeclasses?][]*

This is part one of three in a series of posts in which I will argue that it's
time to consign the `Show` type class to the dustbin of history. I won't
discuss the `Show` class in particular in this post, however; the purpose of
this post is just to establish some background for what I'm going to propose
later in the series.

The ideas in here are already fairly well-trodden ground for Haskellers; little
of what I'm about to say is new. For example:

* [Stack Overflow: Should I use typeclasses or not?][] There are a fair few
  questions along the lines of whether type classes are a good fit for a
  particular problem on Stack Overflow, and most of them have good answers
  (usually: 'no'). This one includes a good set of rules of thumb of when to
  use type classes (and when not to).
* Luke Palmer's article *[Haskell Antipattern: Existential Typeclass][]*
* *[Scrap your type classes][]* from Gabriel Gonzalez' blog, *Haskell for all*.
  The position taken here is a little extreme, but you don't have to agree with
  that position to be able to find the post useful. In particular, the article
  talks about how *Haskell is better at value-level programming,* or
  alternatively stated, how regular old types and functions go a long way.
* After reading *Scrap your type classes,* for an alternative viewpoint, have a
  read of [Edward Kmett's answer to "Are typeclasses essential?"][] on Stack
  Overflow.
* Back when Aeson's error messages only had minimal detail (I think this was
  addressed around aeson 0.9?), I wrote a library which offers an alternative
  `Parser` type, for traversing JSON values and creating Haskell types from the
  contents, and providing better errors when parsing fails. Here's [my response
  to an issue proposing a type class to this library for associating parsers
  with types][].

I would argue that, even though type classes are fantastically useful, it's
actually quite rare to find yourself in a position where introducing a new one
is a good idea. My rules of thumb for deciding whether or not to introduce a
new type class are largely inspired by Gabriel's (see above); in my view, a
type class should preferably:

### Make sense as an abstraction

This means for a class `C a`, it should be possible to write useful functions
of the form `C a => whatever`, and know that these functions will behave
sensibly for any choice of `a` having a `C a` instance. For example, the
function
```
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
```
which performs Kleisli composition of functions of the form `a -> m b`, is
guaranteed to be associative for any choice of `m` having a `Monad m` instance
as a consequence of the `Monad` laws. This means we can do things like
combining a whole bunch of monadic functions with `<=<` without having to worry
about parentheses changing the meaning of the resulting expression.  The
`Monad` class is a perfect example of this concept more generally: the
`Control.Monad` module and the `monad-loops` package contain many more useful
functions with `Monad` constraints.

### Have at least one law

This is closely tied to the previous rule: it is usually the laws of a class
which enable us to write code which is generic over that class and be sure that
they will behave sensibly for any instance.

### Have no more than a few sensible, correct behaviours for a given type (preferably just one)

If there are types which have lots of equally sensible possibilities for their
instances of some class, it becomes harder to write code making use of that
class; we may have to check the instances individually before knowing that
they're safe to use with a given function. In some cases, we may have to create
lots of newtypes to be able to make use of functions which are generic over
the class; at this point, it's better to just use regular functions.

There are a few notable counterexamples for this one. For instance, the
`Monoid` type class is probably one of the most justifiable type classes
around, but it doesn't meet this criterion. Most numeric types, such as `Int`
and `Double`, will have at least two sensible implementations, since addition
and multiplication both form a monoid.

### Have some redundancy in its members' types

I'll illustrate what I mean by this with an example. Take `fmap` from
`Functor`:
```
fmap :: Functor f => (a -> b) -> f a -> f b
```
There are three type variables `f`, `a`, and `b` which appear in this type
signature, and all of them appear more than once (in fact, exactly twice).
This helps us make the most out of the type checker. For example, suppose I
wrote the following:
```
fmap not "hello, world"
```
This won't compile, because `not` expects an argument of type `Bool`, so we can
infer that in this case, the type variable `a` must be instantiated to `Bool`.
However, the second argument, being a `String`, tells us that we will have to
instantiate `a` to `Char`. We can't do both of these together, so type-checking
fails; this is good, because what we had written didn't make sense.  By
contrast, consider
```
fromJSON :: FromJSON a => Data.Aeson.Value -> Data.Aeson.Result a
```
The type variable `a` appears only once (ignoring the constraint), which means
we lose the protection afforded by redundancy. This means that it is much
easier to write something which type-checks fine, but does the completely wrong
thing at runtime. For instance, suppose I was parsing a JavaScript library's
`package.json`, and I wanted to represent the `dependencies` property as a `Map
PackageName VersionRange`. Here's an example of the `dependencies` property in
a `package.json` file:
```
"dependencies": {
  "loose-envify": "^1.1.0",
  "object-assign": "^4.1.1",
  "prop-types": "^15.6.2",
  "scheduler": "^0.11.0"
}
```
But the `FromJSON (Map k v)` instance expects something quite different:
```
"dependencies": [
  ["loose-envify", "^1.1.0"],
  ["object-assign", "^4.1.1"],
  ["prop-types", "^15.6.2"],
  ["scheduler", "^0.11.0"]
]
```
It's very tempting to just write `parseJSON` to parse one of these, but of
course this won't work, because of the above mismatch. I would argue that the
`FromJSON` class makes it far too easy to write code which makes mistakes like
this one.

### Disclaimer

I'm not going to go as far as to argue that the approach I've described here is
the unique correct way to use type classes; there are plenty of other valid
options. For example, if you want to use Template Haskell to generate instances
from your type definitions, such as JSON encoders and decoders, a design
involving type classes like `ToJSON` and `FromJSON` is probably your best bet.
Having said that, the approach I've detailed in this post is the one I try to
follow where I can, and I'm quite confident in its effectiveness.

In the next post, I'm going to look at the `Show` type class with these rules
of thumb in mind.

<!-- Next up: [Part 2: What's wrong with the Show type class](../down-with-show-part-2/) -->

[Definitive guide on when to use typeclasses?]: https://www.reddit.com/r/haskell/comments/1j0awq/definitive_guide_on_when_to_use_typeclasses/
[Haskell Antipattern: Existential Typeclass]: https://lukepalmer.wordpress.com/2010/01/24/haskell-antipattern-existential-typeclass/
[Scrap your type classes]: http://www.haskellforall.com/2012/05/scrap-your-type-classes.html
[Stack Overflow: Should I use typeclasses or not?]: https://stackoverflow.com/questions/17100036/should-i-use-typeclasses-or-not
[Edward Kmett's answer to "Are typeclasses essential?"]: https://stackoverflow.com/questions/25855507/are-typeclasses-essential
[my response to an issue proposing a type class to this library for associating parsers with types]: https://github.com/hdgarrood/aeson-better-errors/issues/4#issuecomment-121549136
