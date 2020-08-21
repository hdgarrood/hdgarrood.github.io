---
layout: post
title: Making full use of PureScript's Generic type class
---

Many PureScript programmers will have made use of the Generic type class at
some point to cut down on the amount of boilerplate needed to define instances
for their data types. However, my impression is that it's less common that
people understand how this process works, or that people know how to write the
code which permits Generic deriving for a particular type class.

In this post, I'm going to assume that you have a vague idea of what the Generic
class is capable of, and you already know how to use it to derive instances for
which Generic deriving is already provided, like Show, Eq, or Argonaut's
EncodeJson and DecodeJson. The goal of this post is to teach you how to write
the code which permits Generic deriving for a particular type class. It draws
inspiration from the [tutorial from the Haskell package generic-deriving][]
(which I also recommend, especially if you use Haskell too).

I'd like to thank the following people for reading draft versions of this post
and providing invaluable feedback: [@yugiohxlight][], [fredrik wallberg
(@quesebifurcan)][], [Dario Oddenino (@dariooddenino)][], [Daniel Brice
(@fried\_brice)][], [Dave Parfitt (@metadave)][], [Willem van den Ende
(@mostalive)][], [Saurabh Nanda (@saurabhnanda)][], and [Donna (@naglalakk)][].

The PureScript code which accompanies this post is published as a Gist, so you
can also [load the code from this post in Try
PureScript](https://try.purescript.org/?gist=c99f9ebf3bec311d1a5065f3fa4bfaa1).

I've included a copy of the imports I'm using here, so that it's clear where
everything comes from:

```purescript
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Generic.Rep as G
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol, SProxy(..))
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafeCrashWith)
```

## Table of Contents

* [Representing data types as sums of products](#representing-data-types-as-sums-of-products)
* [The Generic type class](#the-generic-type-class)
* [Acting on the generic representation types](#acting-on-the-generic-representation-types)
* [Having the compiler generate representation types for us](#having-the-compiler-generate-representation-types-for-us)
* [Storing metadata in the representation types](#storing-metadata-in-the-representation-types)
* [Improving our generic Tree encoding](#improving-our-generic-tree-encoding)


## Representing data types as sums of products

The core idea which enables the Generics library is that data types can be
represented "generically", as "sums of products". What do I mean by that?
Consider the following data types:

```purescript
data Sum a b = Inl a | Inr b
data Product a b = Product a b

-- Type operators for Sum and Product
infixl 6 type Sum as :+:
infixl 7 type Product as :*:

-- Operator for the Product data constructor
infixl 7 Product as :*:
```

We call them "sum" and "product" because they have some common properties with
the familiar `+` and `×` operators which work on numbers, but I'm not going to
go too much into that right now. What is useful to note is that Sum represents
having exactly one of two things (i.e. one or the other, not both), and that
Product represents having both of two things.

Our goal is to find a way of representing as many data types as we can in terms
of Sum and Product. Let's start with a simple one: Maybe. Since there's a
choice between two different cases (Just and Nothing), we'll use Sum. To
represent an absence of information, i.e. the Nothing constructor, we can use
the Unit type, since it only has one inhabitant (unit). We end up with this:

```purescript
type MaybeRep a = Unit :+: a

repFromMaybe :: forall a. Maybe a -> MaybeRep a
repFromMaybe = case _ of
  Nothing -> Inl unit
  Just x -> Inr x

repToMaybe :: forall a. MaybeRep a -> Maybe a
repToMaybe = case _ of
  Inl _ -> Nothing
  Inr x -> Just x
```

Note that repToMaybe and repFromMaybe are each other's inverses, so we can
convert Maybe to and from MaybeRep as much as we like without losing
information.

Let's look at some more examples. Imagine we are wanting to make a website
which allows you to simulate Pokémon battles. To start with, we want to store
information about individual Pokémon, such as the fact that each Pokémon has a
_species_ (such as "Pikachu", "Charizard", "Snorlax"), a _level_, which is an
integer, and a primary and optionally a secondary _type_, such as Normal,
Grass, or Water/Ice (for a Pokémon which has a primary type of Water and a
secondary type of Ice).

```purescript
-- This should really be a sum type but I can't be bothered to write all the
-- constructors out and define conversions to/from String
newtype PokémonType = PokémonType String

-- Auxiliary newtypes for clarity
newtype Species = Species String
newtype Level = Level Int

-- | Fields are Species, level, primary type, secondary type (if any)
data Pokémon = Pokémon Species Level PokémonType (Maybe PokémonType)

-- Some example values
pikachu :: Pokémon
pikachu = Pokémon (Species "Pikachu") (Level 50) (PokémonType "Electric") Nothing

dewgong :: Pokémon
dewgong = Pokémon (Species "Dewgong") (Level 62) (PokémonType "Water") (Just (PokémonType "Ice"))
```

How are we going to represent the Pokémon data type with Sum and Product? In
this case there's only one constructor, so we aren't going to need Sum, but
we do need Product:

```purescript
type PokémonRep = Species :*: Level :*: PokémonType :*: Maybe PokémonType

repFromPokémon :: Pokémon -> PokémonRep
repFromPokémon (Pokémon species level primaryType secondaryType) =
  species :*: level :*: primaryType :*: secondaryType

repToPokémon :: PokémonRep -> Pokémon
repToPokémon (species :*: level :*: primaryType :*: secondaryType) =
  Pokémon species level primaryType secondaryType
```

Again, note that these functions are mutual inverses.

Now let's look at a data type for which we need both Sum and Product. During
battle, Pokémon can be afflicted with a number of statuses, such as being
asleep, poisoned, or paralyzed. If a Pokémon is asleep, we want to keep a
counter for the number of turns they will remain asleep. If a Pokémon is
poisoned, then they should lose some HP each turn. Poison can be normal
severity, in which case the amount of HP they lose each turn is fixed, or it
can be bad severity, in which case the amount of HP they lose each turn
increases over time. For poisoned Pokémon, then, we will want to store both a
severity and a counter for the number of turns they have been poisoned.

```purescript
data PoisonSeverity = NormalPoison | BadPoison

data PokémonStatus
  = Asleep Int
  | Poisoned PoisonSeverity Int
  | Paralyzed
```

The general pattern for converting data type definitions to representations
using Sum and Product is that you represent all of the fields within each
constructor using Product, and then you put all of the constructors together
using Sum. In this case, we end up with:

```purescript
type PokémonStatusRep =
  Int -- Asleep
  :+: (PoisonSeverity :*: Int) -- Poisoned
  :+: Unit -- Paralyzed

repFromPokémonStatus :: PokémonStatus -> PokémonStatusRep
repFromPokémonStatus = case _ of
  Asleep counter -> Inl (Inl counter)
  Poisoned severity counter -> Inl (Inr (severity :*: counter))
  Paralyzed -> Inr unit

repToPokémonStatus :: PokémonStatusRep -> PokémonStatus
repToPokémonStatus = case _ of
  Inl (Inl counter) -> Asleep counter
  Inl (Inr (severity :*: counter)) -> Poisoned severity counter
  Inr _ -> Paralyzed
```

## The Generic type class

The Generic type class captures this pattern of defining an associated generic
representation type for some data type. It's defined like this:

```purescript
class Generic a rep | a -> rep where
  to :: rep -> a
  from :: a -> rep
```

The type `a` is the data type we care about, and the type `rep` is the generic
representation type for `a`; that is, `rep` is a type which contains the same
information as `a`, but is built out of Sums and Products. The functions `to`
and `from` should be mutual inverses, so that `to <<< from` and `from <<< to`
are both the identity function. The functional dependency `a -> rep` says that
there can only be one Generic instance for a given type `a`; the type `a`
determines the type `rep`.

We can define Generic instances for our data types based on the conversions we
defined above:

```purescript
-- Type synonym instances would be really handy here, but sadly we don't have
-- them just yet

-- Generic (Maybe a) (MaybeRep a)
instance genericMaybe :: Generic (Maybe a) (Unit :+: a) where
  to = repToMaybe
  from = repFromMaybe

-- Generic Pokémon PokémonRep
instance genericPokémon :: Generic Pokémon (Species :*: Level :*: PokémonType :*: Maybe PokémonType) where
  to = repToPokémon
  from = repFromPokémon

-- Generic PokémonStatus PokémonStatusRep
instance genericPokémonStatus :: Generic PokémonStatus (Int :+: (PoisonSeverity :*: Int) :+: Unit) where
  to = repToPokémonStatus
  from = repFromPokémonStatus
```

## Acting on the generic representation types

Now that we have seen a few examples of generic representation types, we are
ready to understand how the class can help us avoid boilerplate. The general
idea is that if we are interested in deriving instances for a particular type
class, we can write instances of that type class for just the Sum and Product
types, and then delegate to those instances for any other types we want
instances for. Usually, we use the Generic `from` conversion to convert our
data to Sums and Products, perform whatever work is necessary by making use of
the instances for Sum and Product, and then convert back again at the end with
`to`.

Let's work through an example of this now, with the types we defined above.
Suppose we want to allow people to generically derive instances of the
following type classes for encoding and decoding data in the form of trees of
strings, which might form the basis of a mechanism for serialization or
pretty-printing:

```purescript
data Tree a = Tree a (Array (Tree a))

class TreeEncode a where
  treeEncode :: a -> Tree String

class TreeDecode a where
  treeDecode :: Tree String -> Maybe a
```

Assume that TreeEncode and TreeDecode instances are provided already for the
following types:

* `String`
* `Int`
* `Unit`
* `Maybe a` (given an instance for `a`)

and that we have also provided instances for the newtypes we defined further up
(that is, Species, Level and PokémonType) via `derive newtype instance`.

If we want to encode a Sum, then we need to be able to encode each of the
possibilities for the sum, so we require TreeEncode instances for both
arguments. When encoding, we add a node to the tree to keep track of which side
of the sum we are on.

```purescript
instance treeEncodeSum :: (TreeEncode a, TreeEncode b) => TreeEncode (Sum a b) where
  treeEncode = case _ of
    Inl a -> Tree "Sum:Inl" [treeEncode a]
    Inr b -> Tree "Sum:Inr" [treeEncode b]

instance treeDecodeSum :: (TreeDecode a, TreeDecode b) => TreeDecode (Sum a b) where
  treeDecode = case _ of
    Tree "Sum:Inl" [a] -> Inl <$> treeDecode a
    Tree "Sum:Inr" [b] -> Inr <$> treeDecode b
    _ -> Nothing
```

When we're encoding a Product, we again need to be able to encode both of the
two fields, so we require TreeEncode instances for both of them. We encode to a
tree by creating a branch for each field.

```purescript
instance treeEncodeProduct :: (TreeEncode a, TreeEncode b) => TreeEncode (Product a b) where
  treeEncode (Product a b) = Tree "Product" [treeEncode a, treeEncode b]

instance treeDecodeProduct :: (TreeDecode a, TreeDecode b) => TreeDecode (Product a b) where
  treeDecode = case _ of
    Tree "Product" [a, b] -> Product <$> treeDecode a <*> treeDecode b
    _ -> Nothing
```

Now that we have instances for Sum and Product, we can define instances for the
data types we defined above by having them convert via the associated Rep type.
Specifically, `treeEncode <<< from` allows us to encode, and `map to <<<
treeDecode` allows us to decode. We can tie this all up in a function:

```purescript
genericTreeEncode
  :: forall a rep. Generic a rep => TreeEncode rep => a -> Tree String
genericTreeEncode =
  treeEncode <<< from

genericTreeDecode
  :: forall a rep. Generic a rep => TreeDecode rep => Tree String -> Maybe a
genericTreeDecode =
  map to <<< treeDecode
```

With all of the above in place, the amount of code we need to write to define
instances for a new data type is drastically reduced:

```purescript
instance treeEncodePokémon :: TreeEncode Pokémon where
  treeEncode = genericTreeEncode

instance treeDecodePokémon :: TreeDecode Pokémon where
  treeDecode = genericTreeDecode

instance treeEncodePokémonStatus :: TreeEncode PoisonSeverity where
  treeEncode = genericTreeEncode

instance treeDecodePokémonStatus :: TreeDecode PoisonSeverity where
  treeEncode = genericTreeEncode

instance treeEncodePokémonStatus :: TreeEncode PokémonStatus where
  treeEncode = genericTreeEncode

instance treeDecodePokémonStatus :: TreeDecode PokémonStatus where
  treeDecode = genericTreeDecode
```

Let's test these instances out:

```purescript
main1 :: Effect Unit
main1 = do
  testRoundTrip "pikachu" pikachu
  testRoundTrip "paralyzed" Paralyzed
  testRoundTrip "poisoned" (Poisoned BadPoison 4)

  where
  testRoundTrip ::
    forall a.
    TreeEncode a =>
    TreeDecode a =>
    Eq a =>
    Show a =>
    String -> a -> Effect Unit
  testRoundTrip msg a = do
    log "======"
    log $ "Testing roundtrip: " <> msg
    log $ "Initial value: " <> show a
    log $ "Encoded:       " <> show (treeEncode a)
    let roundTripped = treeDecode (treeEncode a)
    log $ "Round trips successfully? " <> if roundTripped == Just a then "Yes" else "No"
```

Here's the (pretty-printed) output for Pikachu:

```
Tree "Product"
  [ Tree "Product"
    [ Tree "Product"
      [ Tree "Pikachu" []
      , Tree "50" []
      ]
    , Tree "Electric" []
    ]
  , Tree "Nothing" []
  ]
```

So the mechanism works, but defining the representation types and conversions
is very boilerplatey and tiresome. We've just replaced one kind of boilerplate
(instances for encoding and decoding) with another (conversion to and from
generic representation types)!  Fortunately, the compiler can help.

## Having the compiler generate representation types for us

The process of going from a `data` declaration to its generic representation
type is very mechanical and predictable, so it's a good candidate for
automation. The PureScript compiler will generate the appropriate
representation type and conversions for you if you write the appropriate
`derive instance` declarations. For example:

```purescript
derive instance genericPokémon' :: G.Generic Pokémon _
derive instance genericPoisonSeverity' :: G.Generic PoisonSeverity _
derive instance genericPokémonStatus' :: G.Generic PokémonStatus _
```

Note that I've imported the real Generic class qualified as `G`, so as not to
clash with the one we defined above. In this post, everything from the
Data.Generic.Rep module will be under the `G` namespace.


You can inspect a data type's associated representation type using the repl,
like this:

```
> :t (G.to :: _ -> Pokémon)
Constructor "Pokémon" (Product (Argument Species) (Product (Argument Level) (Product (Argument PokémonType) (Argument (Maybe PokémonType))))) -> Pokémon
```

Note that the representation type generated by the compiler is a bit different
to our PokémonRep type. In particular, there are two types we haven't seen
before: Constructor, and Argument.

## Storing metadata in the representation types

The representation types used by the compiler when generating Generic instances
differ a little from the ones we've used up until now. The real generic
representation types are defined in the [Data.Generic.Rep module][], and in
addition to Sum and Product, we also have the following:

* The Constructor type, which is defined as
  ```
  newtype Constructor (name :: Symbol) a = Constructor a
  ```
  The purpose of the Constructor type is to allow type class deriving code to
  see what the names of each of the constructors of the data type in question
  are, which is useful for e.g.  pretty printing, or producing more readable
  JSON encodings.
* The Argument type, which is defined as
  ```
  newtype Argument a = Argument a
  ```
  This type wraps each field in a data constructor, so that when you're writing
  instances, you can easily differentiate Sums and Products from actual
  constructor fields.
* The NoArguments type, which is defined as
  ```
  data NoArguments = NoArguments
  ```
  This data type is used as a more explicit alternative to Unit where a
  constructor has no arguments.
* The NoConstructors type, which is defined as
  ```
  data NoConstructors
  ```
  This type is used as a representation type for data types with no
  constructors at all. For this reason, it has no constructors, and thus no
  inhabitants.

## Improving our generic Tree encoding

Let's rework our generic tree encoding code so that it works with the real
Generic class. Additionally, using the extra information available from the
metadata, let's have it generate trees which correspond a bit better to the
data type definitions in question. In particular, let's use the real
constructor names rather than "Sum:Inl" or "Sum:Inr" to indicate which
constructor we have, and let's also flatten the products for constructors with
multiple fields so that all fields for a particular constructor appear as
direct descendants of the same node in the tree.

We'll start with the flattening of products. For this, we'll use some new type
classes for handling encoding and decoding of all of the arguments to a
particular data constructor.

```purescript
class TreeEncodeArgs a where
  treeEncodeArgs :: a -> Array (Tree String)

class TreeDecodeArgs a where
  treeDecodeArgs :: Array (Tree String) -> Maybe { result :: a, rest :: Array (Tree String) }
```

The TreeDecodeArgs class needs to return a record with `result` and `rest`
fields, because we don't intially know how many elements of the argument array
we will consume during decoding.

For these classes, we'll need instances for the NoArguments, Argument, and
Product types, since those are the ones which appear as arguments to the
Constructor type.

When encoding, we delegate to the TreeEncode instance of each individual field,
and then we use the TreeEncodeArgs class to collect all of the results in a
single flat array:

```purescript
instance treeEncodeArgsNoArguments :: TreeEncodeArgs G.NoArguments where
  treeEncodeArgs _ = []

instance treeEncodeArgsArgument :: TreeEncode a => TreeEncodeArgs (G.Argument a) where
  treeEncodeArgs (G.Argument a) = [treeEncode a]

instance treeEncodeArgsProduct :: (TreeEncodeArgs a, TreeEncodeArgs b) => TreeEncodeArgs (G.Product a b) where
  treeEncodeArgs (G.Product a b) = treeEncodeArgs a <> treeEncodeArgs b
```

When decoding, we do the same, but the other way around:

```purescript
instance treeDecodeArgsNoArguments :: TreeDecodeArgs G.NoArguments where
  treeDecodeArgs = case _ of
    [] -> Just { result: G.NoArguments, rest: [] }
    _ -> Nothing

instance treeDecodeArgsArgument :: TreeDecode a => TreeDecodeArgs (G.Argument a) where
  treeDecodeArgs args = do
    { head, tail: rest } <- Array.uncons args
    result <- G.Argument <$> treeDecode head
    pure { result, rest }

instance treeDecodeArgsProduct :: (TreeDecodeArgs a, TreeDecodeArgs b) => TreeDecodeArgs (G.Product a b) where
  treeDecodeArgs args = do
    { result: a, rest: args1 } <- treeDecodeArgs args
    { result: b, rest: args2 } <- treeDecodeArgs args1
    pure { result: G.Product a b, rest: args2 }
```

Now that we have the flattening of arguments handled, we can move on to better
tagging of sums.

```purescript
instance treeEncodeSum' :: (TreeEncode a, TreeEncode b) => TreeEncode (G.Sum a b) where
  treeEncode (G.Inl a) = treeEncode a
  treeEncode (G.Inr b) = treeEncode b

instance treeDecodeSum' :: (TreeDecode a, TreeDecode b) => TreeDecode (G.Sum a b) where
  treeDecode t = (G.Inl <$> treeDecode t) <|> (G.Inr <$> treeDecode t)
```

These instances might be surprising at first glance. Don't we need to
distinguish the two branches from each other? The answer is that we don't need
to distinguish them at this stage, because the parameters to Sum will include a
Constructor type, and we'll use the information there in order to encode or
decode the appropriate tag. Let's do that now:

```purescript
instance treeEncodeConstructor :: (IsSymbol name, TreeEncodeArgs a) => TreeEncode (G.Constructor name a) where
  treeEncode (G.Constructor a) =
    let
      tag = reflectSymbol (SProxy :: SProxy name)
    in
      Tree tag (treeEncodeArgs a)

instance treeDecodeConstructor :: (IsSymbol name, TreeDecodeArgs a) => TreeDecode (G.Constructor name a) where
  treeDecode (Tree tag args) =
    if tag == reflectSymbol (SProxy :: SProxy name)
      then (G.Constructor <<< _.result) <$> treeDecodeArgs args
      else Nothing
```

The IsSymbol constraint allows us to use reflectSymbol, which "reflects" the
constructor name from the type level down to the value level, allowing us to
store it in the resulting encoded tree, or compare it with the tag in the tree
we're decoding.

Finally, we provide an instance for NoConstructors.

```purescript
instance treeEncodeNoConstructors :: TreeEncode G.NoConstructors where
  treeEncode _ = unsafeCrashWith "unreachable"

instance treeDecodeNoConstructors :: TreeDecode G.NoConstructors where
  treeDecode _ = Nothing
```

Now that we have the right instances for all of the real generic representation
types, we can write versions of `genericTreeEncode` and `genericTreeDecode`
which use the real Generic class and the metadata types:

```
genericTreeEncode'
  :: forall a rep. G.Generic a rep => TreeEncode rep => a -> Tree String
genericTreeEncode' =
  treeEncode <<< G.from

genericTreeDecode'
  :: forall a rep. G.Generic a rep => TreeDecode rep => Tree String -> Maybe a
genericTreeDecode' =
  map G.to <<< treeDecode
```

Note that these are basically the same as the `genericTreeEncode` and
`genericTreeDecode` functions we already defined, except we are using the
real Generic class from Data.Generic.Rep.

Phew! Let's check that works:

```purescript
main2 :: Effect Unit
main2 = do
  testRoundTrip "pikachu" pikachu
  testRoundTrip "paralyzed" Paralyzed
  testRoundTrip "poisoned" (Poisoned BadPoison 4)

  where
  testRoundTrip ::
    forall a rep.
    G.Generic a rep =>
    TreeEncode rep =>
    TreeDecode rep =>
    Eq a =>
    Show a =>
    String -> a -> Effect Unit
  testRoundTrip msg a = do
    log "======"
    log $ "Testing roundtrip: " <> msg
    log $ "Initial value: " <> show a
    log $ "Encoded:       " <> show (genericTreeEncode' a)
    let roundTripped = genericTreeDecode' (genericTreeEncode' a)
    log $ "Round trips successfully? " <> if roundTripped == Just a then "Yes" else "No"
```

And yes, that's giving us a much nicer encoding:

```
Tree "Pokémon"
  [ Tree "Pikachu" []
  , Tree "50" []
  , Tree "Electric" []
  , Tree "Nothing" []
  ]
```

Hopefully you now have a slightly better understanding of why the Generic class
looks the way it does, and what exactly is going on underneath, so that you can
write your own Generic deriving mechanisms and cut right through all that
boilerplate you've been struggling with. For further inspiration and examples
on how to use Generic to derive type class instances, I recommend reading the
[generics-rep][] and [argonaut-generic][] packages. Happy deriving!

[generics-rep library]: https://pursuit.purescript.org/packages/purescript-generics-rep
[tutorial from the Haskell package generic-deriving]: https://hackage.haskell.org/package/generic-deriving-1.13.1/docs/Generics-Deriving-Base.html
[Data.Generic.Rep module]: https://pursuit.purescript.org/packages/purescript-generics-rep/docs/Data.Generic.Rep
[argonaut-generic]: https://pursuit.purescript.org/packages/purescript-argonaut-generic
[generics-rep]: https://pursuit.purescript.org/packages/purescript-generics-rep

[@yugiohxlight]: https://twitter.com/yugiohxlight
[fredrik wallberg (@quesebifurcan)]: https://twitter.com/quesebifurcan
[Dario Oddenino (@dariooddenino)]: https://twitter.com/dariooddenino
[Daniel Brice (@fried_brice)]: https://twitter.com/fried_brice
[Dave Parfitt (@metadave)]: https://twitter.com/metadave
[Willem van den Ende (@mostalive)]: https://twitter.com/mostalive
[Saurabh Nanda (@saurabhnanda)]: https://twitter.com/saurabhnanda
[Donna (@naglalakk)]: https://twitter.com/naglalakk
