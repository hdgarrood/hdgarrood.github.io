---
layout: post
title: How to write your own Generic deriving mechanisms
draft: true
---

It is a truth universally acknowledged that the Generic class from the
[generics-rep library][] is extremely cool and useful, but it can be a little
difficult to understand, especially for people who haven't already come across
GHC's version. I hope with this post I can shed a little more light and enable
you to understand Generic well enough to create your own mechanisms for
deriving instances. This tutorial draws inspiration from the [tutorial from the
Haskell package generic-deriving][].

This post is written as a literate PureScript module. It's published as a Gist,
so you can also [load the code from this post in Try
PureScript](https://try.purescript.org/?gist=c99f9ebf3bec311d1a5065f3fa4bfaa1).

```purescript
module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Generic.Rep as G
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol, SProxy(..))
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafeCrashWith)

main :: Effect Unit
main = do
  -- these will be defined further down
  log "main1"
  main1
  log "main2"
  main2
```

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
a choice between two things (OR), and that Product represents having both of
two things (AND).

Our goal is to find a way of representing as many data types as we can in terms
of Sum and Product. Let's start with a simple one: Maybe. Since there's a
choice between two different cases (Just and Nothing), we'll use Sum. To
represent an absence of information, i.e. the Nothing constructor, we can use
the Unit type, since it only has one inhabitant (unit). We end up with this:

```purescript
type MaybeRep a = Unit :+: a

maybeToRep :: forall a. Maybe a -> MaybeRep a
maybeToRep = case _ of
  Nothing -> Inl unit
  Just x -> Inr x

maybeFromRep :: forall a. MaybeRep a -> Maybe a
maybeFromRep = case _ of
  Inl _ -> Nothing
  Inr x -> Just x
```

Note that maybeToRep and maybeFromRep are each other's inverses, so we can
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

pokémonToRep :: Pokémon -> PokémonRep
pokémonToRep (Pokémon species level primaryType secondaryType) =
  species :*: level :*: primaryType :*: secondaryType

pokémonFromRep :: PokémonRep -> Pokémon
pokémonFromRep (species :*: level :*: primaryType :*: secondaryType) =
  Pokémon species level primaryType secondaryType
```

Again, note that these functions are mutual inverses. Now let's look at a data
type for which we need both Sum and Product. During battle, Pokémon can be
afflicted with a number of statuses, such as being asleep, poisoned, or
paralyzed. If a Pokémon is asleep, we want to keep a counter for the number of
turns they will remain asleep. If a Pokémon is poisoned, then they should lose
some HP each turn. Poison can be normal severity, in which case the amount of
HP they lose each turn is fixed, or it can be bad severity, in which case the
amount of HP they lose each turn increases over time. For poisoned Pokémon,
then, we will want to store both a severity and a counter for the number of
turns they have been poisoned.

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

pokémonStatusToRep :: PokémonStatus -> PokémonStatusRep
pokémonStatusToRep = case _ of
  Asleep counter -> Inl (Inl counter)
  Poisoned severity counter -> Inl (Inr (severity :*: counter))
  Paralyzed -> Inr unit

pokémonStatusFromRep :: PokémonStatusRep -> PokémonStatus
pokémonStatusFromRep = case _ of
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

The type `a` is the data type we care about, and the type `rep` is a generic
representation type which should be isomorphic to that type, in the sense that
we can write conversion functions from `a` to `rep` and back without losing
information. The functional dependency `a -> rep` says that there can only be
one Generic instance for a given type `a`; the type `a` determines the type
`rep`.

We can define Generic instances for our data types based on the conversions we
defined above:

```purescript
-- Type synonym instances would be really handy here, but sadly we don't have
-- them just yet

-- Generic (Maybe a) (MaybeRep a)
instance genericMaybe :: Generic (Maybe a) (Unit :+: a) where
  to = maybeFromRep
  from = maybeToRep

-- Generic Pokémon PokémonRep
instance genericPokémon :: Generic Pokémon (Species :*: Level :*: PokémonType :*: Maybe PokémonType) where
  to = pokémonFromRep
  from = pokémonToRep

-- Generic PokémonStatus PokémonStatusRep
instance genericPokémonStatus :: Generic PokémonStatus (Int :+: (PoisonSeverity :*: Int) :+: Unit) where
  to = pokémonStatusFromRep
  from = pokémonStatusToRep
```

## Acting on the generic representation types

Now, if we want to easily derive instances of a particular type class, then we
can start by writing instances of that type class for just the Sum and Product
types. This will then allow us to easily write instances for many more data
types, as long as we have Generic instances for those types. The general idea
is that we use the Generic conversion to convert our data to Sums and Products,
perform whatever work is necessary by making use of the instances for Sum and
Product, and then convert back again at the end.

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

We'll need some instances for basic types first:

```purescript
instance treeEncodeString :: TreeEncode String where
  treeEncode x = Tree x []

instance treeEncodeInt :: TreeEncode Int where
  treeEncode = treeEncode <<< show

instance treeDecodeString :: TreeDecode String where
  treeDecode = case _ of
    Tree s [] -> Just s
    _ -> Nothing

instance treeDecodeInt :: TreeDecode Int where
  treeDecode = Int.fromString <=< treeDecode

instance treeEncodeUnit :: TreeEncode Unit where
  treeEncode _ = Tree "Unit" []

instance treeDecodeUnit :: TreeDecode Unit where
  treeDecode = case _ of
    Tree "Unit" [] -> Just unit
    _ -> Nothing

instance treeEncodeMaybe :: TreeEncode a => TreeEncode (Maybe a) where
  treeEncode = case _ of
    Just a -> Tree "Just" [treeEncode a]
    Nothing -> Tree "Nothing" []

instance treeDecodeMaybe :: TreeDecode a => TreeDecode (Maybe a) where
  treeDecode = case _ of
    Tree "Just" [a] -> Just <$> treeDecode a
    Tree "Nothing" [] -> Just Nothing
    _ -> Nothing

derive newtype instance treeEncodeSpecies :: TreeEncode Species
derive newtype instance treeDecodeSpecies :: TreeDecode Species

derive newtype instance treeEncodeLevel :: TreeEncode Level
derive newtype instance treeDecodeLevel :: TreeDecode Level

derive newtype instance treeEncodePokémonType :: TreeEncode PokémonType
derive newtype instance treeDecodePokémonType :: TreeDecode PokémonType

instance treeEncodePoisonSeverity :: TreeEncode PoisonSeverity where
  treeEncode s = treeEncode case s of
    NormalPoison -> "NormalPoison"
    BadPoison -> "BadPoison"

instance treeDecodePoisonSeverity :: TreeDecode PoisonSeverity where
  treeDecode = fromString <=< treeDecode
    where
    fromString = case _ of
      "NormalPoison" -> Just NormalPoison
      "BadPoison" -> Just BadPoison
      _ -> Nothing
```

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
  testRoundTrip :: forall a. TreeEncode a => TreeDecode a => String -> a -> Effect Unit
  testRoundTrip msg a = do
    log $ "Testing roundtrip: " <> msg
    traceM a
    traceM (treeEncode a)
    traceM (treeDecode (treeEncode a) :: Maybe a)
```

Here's the output for Pikachu:

```
Tree {
  value0: 'Product',
  value1: [
    Tree {
      value0: 'Product',
      value1: [
        Tree {
          value0: 'Product',
          value1: [
            Tree { value0: 'Pikachu', value1: [] },
            Tree { value0: '50', value1: [] }
          ]
        },
        Tree { value0: 'Electric', value1: [] }
      ]
    },
    Tree { value0: 'Nothing', value1: [] }
  ]
}
```

So the mechanism works, but defining the representation types and conversions
is very boilerplatey and tiresome. We've just replaced one kind of boilerplate
with another! Fortunately, the compiler can help.

## Having the compiler generate representation types for us

The process of going from a `data` declaration to its generic representation
type is very mechanical and predictable, so it's a good candidate for
automation. The PureScript compiler will generate the appropriate
representation type and conversions for you if you write e.g.

```purescript
derive instance genericPokémon' :: G.Generic Pokémon _
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
  constructors at all. For this reason, it has no constructors; it's isomorphic
  to Void.

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
fields because we don't initially know how many elements of the argument array
decoding is going to consume.

For these classes, we'll need instances for the NoArguments, Argument, and
Product types, since those are the ones which appear inside the Constructor
type.

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

Phew! Let's check that works:

```purescript
main2 :: Effect Unit
main2 = do
  testRoundTrip "pikachu" pikachu
  testRoundTrip "paralyzed" Paralyzed
  testRoundTrip "poisoned" (Poisoned BadPoison 4)

  where
  testRoundTrip :: forall a rep. G.Generic a rep => TreeEncode rep => TreeDecode rep => String -> a -> Effect Unit
  testRoundTrip msg a = do
    log $ "Testing roundtrip: " <> msg
    traceM a
    traceM (treeEncode (G.from a))
    traceM (map G.to (treeDecode (treeEncode (G.from a))) :: Maybe a)
```

And yes, that's giving us a much nicer encoding:

```
Tree {
  value0: 'Pokémon',
  value1: [
    Tree { value0: 'Pikachu', value1: [] },
    Tree { value0: '50', value1: [] },
    Tree { value0: 'Electric', value1: [] },
    Tree { value0: 'Nothing', value1: [] }
  ]
}
```

Hopefully you now have a slightly better understanding of why the Generic class
looks the way it does, and what exactly is going on underneath, so that you can
write your own Generic deriving mechanisms and cut right through all that
boilerplate you've been struggling with. For further inspiration and examples
on how to use Generic to derive type class instances, I recommend reading the
[generics-rep][] and [argonaut-codecs][] packages. Happy deriving!

[generics-rep library]: https://pursuit.purescript.org/packages/purescript-generics-rep
[tutorial from the Haskell package generic-deriving]: https://hackage.haskell.org/package/generic-deriving-1.13.1/docs/Generics-Deriving-Base.html
[Data.Generic.Rep module]: https://pursuit.purescript.org/packages/purescript-generics-rep/docs/Data.Generic.Rep
[argonaut-codecs]: https://pursuit.purescriptorg/packages/purescript-argonaut-codecs
[generics-rep]: https://pursuit.purescriptorg/packages/purescript-generics-rep
