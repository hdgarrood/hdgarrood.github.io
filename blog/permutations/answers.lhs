A colleague of mine severely nerd-sniped me at work recently with this rather
innocuous-sounding challenge. I think it's a rather lovely little exercise and
decided to write up my solution.

This post is designed to be read as a programming exercise. It will guide you
along as it helps you break the problem into parts. If you want, you can skip
to the end and read all the solutions (but that would be cheating).

Download the [literate haskell source file](permutations.lhs) first; you can
fill in the gaps and then compile it or load it into GHCi (with `:l
permutations.lhs`, like any other haskell file.

Consider the following shuffling technique:

1. Take one card from the top of the deck and discard it into a second pile.
2. Take another card from the top of the deck, and put it at the bottom of the
   deck.
3. Repeat these two steps, putting all discarded cards from step 1 into the
   same pile, until the original deck is all gone and the second pile has all
   the cards in it.

For example, suppose we have a deck with 5 cards. The process looks like this:

{% include permutations/shuffle-viz.html %}

The problem is: how many shuffles does it take until a deck is in the same
order as when you started, for a deck with an arbitrary number of cards? Write
a function, `f :: Int -> Int`, such that `f n` is the minimum number of
shuffles required to do this.

We're going to use Haskell, because this is all about *functions* (in the
mathematical sense), and so Haskell, being a *functional programming language*,
is especially well suited to the job.

> {-# OPTIONS_GHC -Wall #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> module Main where
>
> import Control.Monad (forM_)
> import Data.Maybe (catMaybes)
> import Data.List (nub, sort)
> import System.Environment (getArgs)
> import Test.QuickCheck hiding (infiniteList)
>
> todo :: a
> todo = error "todo"

A `Card` is represented as an `Int`, except that the type checker should ensure
that we don't put a `Card` where an `Int` should go, or vice versa.

> newtype Card = Card Int
>     deriving (Eq, Ord, Arbitrary)
>
> unCard :: Card -> Int
> unCard (Card n) = n
>
> instance Show Card where
>     show = show . unCard

We will represent a deck of cards as a list. The head of the list will
represent the top of the deck.

> type Deck = [Card]
>
> unCardAll :: Deck -> [Int]
> unCardAll = map unCard
>
> makeDeck :: Int -> Deck
> makeDeck n = map Card [1..n]

Our first job is to define a function that only performs steps 1 and 2 of the
shuffle. What type should this function be? It should take a deck and a new
pile, and return an updated deck and pile, so let's go with `(Deck, Deck) ->
(Deck, Deck)`.

> step :: (Deck, Deck) -> (Deck, Deck)
> step (deck, pile) = case deck of
>     a:b:cs -> (cs ++ [b], a : pile)
>     a:cs   -> (cs, a : pile)
>     []     -> ([], pile)

Now, we should think about what properties our `step` function should satisfy,
so that we can get QuickCheck to test them for us. Here's one: after performing
`step` n times on a deck, we should end up with the same number of cards that
we started with:

> prop_step_sameLength :: Deck -> Int -> Bool
> prop_step_sameLength deck n' =
>     sumLength (times n step (deck, [])) == length deck
>     where
>     n = (abs n') `mod` (length deck + 1)
>     times m f z = iterate f z !! m
>     sumLength (a, b) = length a + length b

Here's another: after one step, we should have one fewer card in the deck
(unless we started with an empty deck, in which case we should still have an
empty deck)

> prop_step_oneFewer :: Deck -> Bool
> prop_step_oneFewer deck =
>     length (fst (step (deck, []))) == newLength deck
>     where
>     newLength [] = 0
>     newLength d = (length d) - 1

Write `step` and make sure it satisfies these properties before continuing!
You can run the tests by typing `runhaskell permutations.lhs`, or by loading it
into GHCi and typing `main`.

Next we need to write a function, `shuffle`, that performs the shuffle on a
deck. So its type should be `Deck -> Deck`. Here are some Prelude functions
that might come in handy:

* `dropWhile :: (a -> Bool) -> [a] -> [a]`: drops elements from the front of a
  list until it finds an element that satisfies a predicate.
* `iterate :: (a -> a) -> a -> [a]`: successively applies a function to a
  value, and returns all the intermediate values. So `iterate f x` is `[x, f x,
  f (f x)...]`

> first :: (a -> Bool) -> [a] -> a
> first p = head . dropWhile (not . p)
>
> shuffle :: Deck -> Deck
> shuffle d = snd . first (null . fst) $ iterate step (d, [])

More test properties: shuffling a deck should return another deck with the same
number of cards:

> prop_shuffle_sameLength :: Deck -> Bool
> prop_shuffle_sameLength deck = length (shuffle deck) == length deck

Shuffling a deck should move the top card to the bottom:

> prop_shuffle_topToBottom :: Deck -> Bool
> prop_shuffle_topToBottom [] = True
> prop_shuffle_topToBottom deck@(topCard:_) =
>     last (shuffle deck) == topCard

Next up is a function `order` which, given a function, gives us the number of
times we have to apply it to a given value to get that value again. So its type
should be `Eq a => (a -> a) -> a -> Int`. We need the `Eq` constraint so that
we can test values to see if they're the same as the first one.

> order :: Eq a => (a -> a) -> a -> Int
> order f z = (+1) . length . takeWhile (/= z) . drop 1 $ iterate f z

To test `order`: if we have a function that subtracts 1 from positive numbers
and returns n otherwise, the number of times we have to apply it to `n` to get `n` again should be `n + 1`:

> prop_order_subtractOne :: Int -> Bool
> prop_order_subtractOne n' = order f n == n + 1
>     where
>     n = abs n' + 1 -- ensure that n >= 1
>     f x | x > 0     = x - 1
>         | otherwise = n

For our first attempt at answering the question, we can use a naive solution,
which is just to keep shuffling the deck until we get the same deck again.
We've got all the building blocks now, and all that remains is to put them
together.

> f1 :: Int -> Int
> f1 = order shuffle . makeDeck

`f1` is easier to test by looking at particular cases. This code gives a few
inputs and expected outputs for `f`.  If you run the tests (with `runhaskell
permutations.lhs`, as before) it will check that your `f1` works for all of
these values.

> examples_f :: [(Int, Int)]
> examples_f =
>     [ (4, 2)
>     , (5, 5)
>     , (52, 510)
>     , (53, 53)
>     , (100, 120)
>     ]

Now try doing `f1 200`, which should give you 8460. Notice how long it takes to
compute. We can do much better than this, but in order to improve our
implementation, we need to do some maths.

`shuffle` is a function of type `Deck -> Deck`, but we can also imagine it like
a function `S -> S`, where `S` is the set of natural numbers from 1 up to n.
Let's call this new function `g`. `g` takes the initial position of a card in
the deck, and gives you the position after shuffling the deck once.  So in the
case where n = 5, we have:

```
g :: S -> S
g x = case x of
    1 -> 5 -- The card on top goes to the bottom
    2 -> 1 -- The card second from the top goes to the top
    3 -> 4 -- and so on
    4 -> 2
    5 -> 3
```

What do we know about this function?

Firstly, we know that this function must be *injective*, that is, each output
corresponds to exactly one input. This is true because we can't end up with two
cards in the same position after shuffling.

We also know that it must be *surjective*, which means that for every position
in the deck (ie every number from 1 up to n), after a shuffle, there must be
one card that ends up at that position. If we have a deck of 5 cards, and we
shuffle it, we must always end up with a deck where there is a card in the 1st
position, and a card in the 2nd, 3rd, 4th, and 5th.

Another thing we know about this function is that it its *domain* (the set of
values that it accepts as input) and its *range* (the set of values that its
output is in) are the same. In our case `g` has `S` as its domain and its
range.

A function that is both injective and surjective is called a *bijective
function*, and a bijective function whose domain and range are the same is
called a *permutation*. Permutations have some really nice properties, and
knowing about these will help us write a correct and efficient program.

Here's another way of writing `g`:

```
g = (1 5 3 4 2)
```

This is called *cycle notation*, and is a useful way of writing permutations.
It says that `g` takes 1 to 5, 5 to 3, 3 to 4, 4 to 2, and 2 back to 1.

We can tell from this notation that `g` has an order of 5, because the cycle
has 5 numbers in it. Each time we apply `g`, we move the cycle around by 1
step; therefore moving the cycle around 5 times gets us back to where we
started.

What about the permutation for a deck of 8 cards? In Haskell, it would look
like this:

```
g :: S -> S
g x = case x of
    1 -> 8
    2 -> 4
    3 -> 7
    4 -> 2
    5 -> 6
    6 -> 3
    7 -> 5
    8 -> 1
```

In this case, `g` takes 1 to 8, and 8... back to 1. What can we do when the
cycle doesn't have all of the numbers in it?

The answer is to take the next number that isn't in any of our cycles and make
a new one. So given that one of the cycles in `g` is `(1 8)`, we can start with
2, to get another cycle: `(2 4)`. We are still missing 3, so start with 3 to
get another cycle: `(3 7 5 6)`. Now we're done; we have 3 cycles which, when
put together, tell us what happens to each of the numbers 1 to 8:

```
g = (1 8)(2 4)(3 7 5 6)
```

Since no number appears in more than one of these cycles (another way of saying
this is that they are *disjoint*), we can consider each of them individually.

The first cycle has two elements, so it must have an order of 2. Does that mean
`g` has an order of two? No, because applying `g` twice to 3 gives us 5.

We know that `(1 8)` on its own has an order of 2, and so does `(2 4)`.
However, `(3 7 5 6)` has an order of 4. What's the minimum number of times we
have to apply `g` to get all of these back to where they started?

The answer is the least common multiple of all of the cycle lengths. So in this
case, it's 4.

So now we have a new way of calculating the order of the shuffle for a given
deck size: do the shuffle once, use the resulting deck to work out how to
represent the shuffle as a set of disjoint cycles, and then get the least
common multiple of the cycle lengths.

First we need a way of representing a cycle in Haskell. Let's go with this:

> newtype Cycle = Cycle [Int]
>     deriving (Eq, Ord)
>
> instance Show Cycle where
>     show (Cycle xs) = "(" ++ join " " (map show xs) ++ ")"
>         where
>         join glue (y:ys) = y ++ (concatMap (glue ++) ys)
>         join _ [] = ""

So a `Cycle` is just a list of Ints. So the cycle for `g` when n = 5 would be:

```
g = Cycle [1,5,3,4,2]
```

There's a small problem here: suppose we make another `Cycle` from the list
`[5,3,4,2,1]`. This `Cycle` still takes 1 to 5, 5 to 3, and so on, like `g`.
So we should consider them to be the same. However, Haskell will look at the
inner list to decide whether two cycles are equal. Because the lists are
different, Haskell will think that the cycles are different.

We can get around this issue by saying that a Cycle should always start with
its smallest element. So the second representation of `g` above would be
invalid.

A good way of implementing this in Haskell is to define a *smart constructor*:
a function like `Cycle` whose type is `[Int] -> Cycle`, but which makes sure
our statement above holds. Then, as long as we remember to use our smart
constructor rather than `Cycle`, we'll be ok.

If we were writing a proper program, we would probably define `Cycle` and its
smart constructor in a separate module and then only export the smart
constructor, to ensure that we don't make this mistake. Here it's probably not
worth the effort.

The next task is to write this smart constructor function. Let's call it
`makeCycle`. It should take an infinite list of cycling values, cut it off at
the first instance where a value is repeated, and then return a `Cycle` where
the smallest value comes first.

You might find it useful to define a function, `rotate`, which takes a list and
moves one element from the front to the back.

> rotate :: [a] -> [a]
> rotate (x:xs) = xs ++ [x]
> rotate [] = []

> makeCycle :: [Int] -> Cycle
> makeCycle (x:xs) =
>     let ys    = x : takeWhile (/= x) xs
>         h     = minimum ys
>         elems = first (\(y:_) -> y == h) (iterate rotate ys)
>     in  Cycle elems
> makeCycle [] = Cycle []

To test `makeCycle`: if we take an infinite periodically cycling list, drop
some arbitrary number of elements from the front, and call `makeCycle` on it,
we should get the same cycle as if we hadn't dropped any.

> prop_makeCycle_drop :: [Int] -> Int -> Bool
> prop_makeCycle_drop list' n' =
>     makeCycle infiniteList == makeCycle (drop n infiniteList)
>     where
>     n = abs n' + 1   -- ensure n >= 1
>     list = 1 : list' -- ensure list is nonempty
>     infiniteList = cycle . nub $ list

Next: write a function that takes a `Cycle` and returns its length.

> cycleLength :: Cycle -> Int
> cycleLength (Cycle n) = length n

Now write a function that takes a deck size, n, and returns the *graph* of
the permutation for shuffling the deck with n cards, as a list of pairs mapping
inputs to outputs. So for each input from 1 up to n there should be one pair
in the result containing the input and the corresponding output. The order
doesn't matter.

> type PermutationGraph = [(Int, Int)]
>
> permutation :: Int -> PermutationGraph
> permutation n = go 0 (unCardAll . shuffle . makeDeck $ n)
>     where go m (x:xs) = (x, m+1) : go (m+1) xs
>           go _ [] = []

Here are some example inputs and outputs. (I've used `sort` so that the order
doesn't matter). This says that, for example, the permutation graph for 4
should be `[(4,1),(2,2),(3,3),(1,4)]`. Again, these examples are included in
the tests.

> examples_permutation :: [(Int, PermutationGraph)]
> examples_permutation =
>     [ (4, sort [(4,1),(2,2),(3,3),(1,4)])
>     , (5, sort [(2,1),(4,2),(5,3),(3,4),(1,5)])
>     , (8, sort [(8,1),(4,2),(6,3),(2,4),(7,5),(5,6),(3,7),(1,8)])
>     ]

Now we need to implement the algorithm I described earlier for decomposing a
permutation into a product of disjoint cycles. First we should write a function
that extracts one `Cycle` from a `PermutationGraph`. It should:

* take a `PermutationGraph`,
* look at the first input in the graph, and see what it maps to,
* find where that output occurs as an input in the graph, and see where
  *that* maps to,
* keep going, producing an infinite cycling list of all of these values, and
* turn that list into a `Cycle`.

The infinite list should repeat periodically with all the values in a cycle;
that is, the kind of value that `makeCycle` is expecting to get. So we've
already done the last step of implementing this function; we just need to apply
`makeCycle` to that infinite list.

> extractCycle :: PermutationGraph -> Cycle
> extractCycle xs@(h:_) = makeCycle $ go h xs
>     where
>     go (y, pos) ys = case lookup pos ys of
>         Just z -> y : go (pos, z) ys
>         Nothing -> cycle [y, pos]
> extractCycle [] = makeCycle []

Next up is a function that can extract *all* the cycles from a permutation
graph.  Your implementation should use `extractCycle` repeatedly to get all of
the cycles out of the list.

> decompose :: PermutationGraph -> [Cycle]
> decompose xs = foldl f [] $ take (length xs) (iterate rotate xs)
>     where
>     f acc ys =
>         let cyc = extractCycle ys
>         in if cyc `elem` acc
>             then acc
>             else cyc : acc

Another QuickCheck property: The sum of cycle lengths after decomposing the
permutation graph for a given integer n should equal n:

> prop_decompose_sumCycleLengths :: Int -> Bool
> prop_decompose_sumCycleLengths n' = sumCycleLengths n == n
>     where
>     n = abs n' + 1
>     sumCycleLengths = sum . map cycleLength . decompose . permutation

Once we have decomposed a permutation into a product of disjoint cycles, the
final step is finding the least common multiple of their lengths. Write a
function that takes a list of `Int`, and returns the least common multiple of
all of them.  The Prelude gives us `lcm`, but it works on two numbers. You
might find it helpful here.

> lcm' :: [Int] -> Int
> lcm' = foldl lcm 1

We have all the building blocks now: write another implementation of `f` using
`permutation`, `decompose`, `cycleLength`, and `lcm'`.

> f2 :: Int -> Int
> f2 = lcm' . map cycleLength . decompose . permutation

Now that we have two different implementations of `f`, we can test to see
whether we got them both right by seeing if they are equal. Because their
implementations are quite different this will probably be quite a good
indication of whether we got it right.

> prop_f1_f2_identical :: Int -> Bool
> prop_f1_f2_identical x = f1 x == f2 x

You did it! See how much faster `f2` is? My answers are [here](answers.lhs) if
you want to compare them.

Below here is code that you don't need to worry about. This is a function that
takes a set of test inputs and expected outputs, a function mapping inputs to
outputs, and returns an action that checks whether the expected outputs are the
same as the actual outputs, and prints messages to the console if they are not.

> testByExamples :: (Show a, Show b, Eq b) => [(a, b)] -> (a -> b) -> IO ()
> testByExamples examples f =
>     let fails = catMaybes $ map runExample examples
>     in if null fails
>         then putStrLn "+++ OK, passed all examples."
>         else forM_ fails printFailure
>     where
>     runExample (input, expected) =
>         let actual = f input
>         in if expected == actual
>             then Nothing
>             else Just $ (input, expected, actual)
>     printFailure (input, expected, actual) =
>         putStrLn $ concat
>             [ "failed on input "
>             , show input
>             , ": expected <"
>             , show expected
>             , ">, got <"
>             , show actual
>             , ">."
>             ]

This is the definition of the program. It says that if we get a number as an
argument, calculate `f2` for that number. Otherwise run the tests.

> main :: IO ()
> main = do
>     args <- getArgs
>     case args of
>         x:_ -> do
>             print . f2 . read $ x
>         _   -> do
>             let qc :: Testable a => a -> String -> IO ()
>                 qc prop msg =
>                     putStrLn ("Testing: " ++ msg) >>
>                         quickCheck (sensible prop)
>             let te exs f msg =
>                     putStrLn ("Testing: " ++ msg) >>
>                         testByExamples exs f
>             qc prop_step_sameLength "prop_step_sameLength"
>             qc prop_step_oneFewer "prop_step_oneFewer"
>             qc prop_shuffle_sameLength "prop_shuffle_sameLength"
>             qc prop_shuffle_topToBottom "prop_shuffle_topToBottom"
>             qc prop_order_subtractOne "prop_order_subtractOne"
>             te examples_f f1 "examples for f1"
>             qc prop_makeCycle_drop "prop_makeCycle_drop"
>             te examples_permutation
>                   (sort . permutation) "examples for permutation"
>             qc prop_decompose_sumCycleLengths
>                   "prop_decompose_sumCycleLengths"
>             te examples_f f2 "examples for f2"
>             qc prop_f1_f2_identical "prop_f1_f2_identical"
>     where
>     sensible :: Testable a => a -> Property
>     sensible = mapSize (floor . logBase (2 :: Double) . fromIntegral)
