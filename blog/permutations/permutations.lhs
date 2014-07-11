
A colleague of mine severely [nerd-sniped][] me at work recently with this
rather innocuous-sounding challenge. I think it's a rather lovely little
exercise and decided to write up my solution.

This post is designed to be read as a programming exercise. It will guide you
along as it helps you break the problem into parts. If you want, you can skip
to the end and read all the solutions&mdash;but that would be cheating.

The problem is as follows:

Given the following shuffling technique:

1. Take one card from the top of the deck and discard it into a second pile.
2. Take another card from the top of the deck, and put it at the bottom of the
   deck.
3. Repeat these two steps, putting all discarded cards from step 1 into the
   same pile, until the original deck is all gone and the second pile has all
   the cards in it.

For example, suppose we have a deck with 5 cards. The process looks like this:

Original deck     Second pile
    =============     ===========
     [A 2 3 4 5]      [         ] Initial state

    Take Ace from the top and discard

     [  2 3 4 5]      [        A]

    Take 2 and put it on the bottom of the deck

     [  3 4 5 2]      [        A]

     [    4 5 2]      [      3 A] Take 3 from the top and discard
     [    5 2 4]      [      3 A] Take 4 and put it on the bottom of the deck
     [      2 4]      [    5 3 A] Take 5 and discard
     [      4 2]      [    5 3 A] Take 2 and put it on the bottom of the deck
     [        2]      [  4 5 3 A]
     [         ]      [2 4 5 3 A]

How many shuffles does it take until the deck is in the same order as when
you started?

We're going to use Haskell, because this is all about *functions* (in the
mathematical sense), and so Haskell, being a *functional programming language*,
is especially well suited to the job.

Let's start with this:

> {-# OPTIONS_GHC -Wall #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> module Main where
>
> import Data.List (sort, elemIndex)
> import Data.Maybe (catMaybes)
> import System.Environment (getArgs)
> import Test.QuickCheck

A `Card` is just an `Int`, except that the type checker should ensure that we
don't put a `Card` where an `Int` should go, or vice versa.

> newtype Card = Card Int
>     deriving (Eq, Ord, Arbitrary)
>
> unCard :: Card -> Int
> unCard (Card n) = n
>
> instance Show Card where
>     show = show . unCard

We will represent a deck of cards just as a list. The head of the list will
represent the top of the deck.

> type Deck = [Card]
>
> unCardAll :: Deck -> [Int]
> unCardAll = map unCard
>
> makeDeck :: Int -> Deck
> makeDeck n = map Card [1..n]

### Task 1: `step`

To make this problem easier, we need to split it into smaller problems first.
Therefore our first job is to define a function that only performs steps 1 and
2 of the shuffle. What type should this function be? It should take a deck and
a new pile, and return an updated deck and pile, so let's go with `(Deck,
Deck)`, `(Deck, Deck)`.

> step :: (Deck, Deck) -> (Deck, Deck)
> step (deck, pile) = case deck of
>     a:b:cs -> (cs ++ [b], a : pile)
>     a:cs   -> (cs, a : pile)
>     []     -> ([], pile)

-- After performing n steps on a deck, we should end up with the same
-- number of cards as we started with
prop_step_sameLength :: Deck -> Int -> Bool
prop_step_sameLength deck n' =
    sumLength (times n step (deck, [])) == length deck
    where
    n = abs n'
    times m f z = iterate f z !! m
    sumLength (a, b) = length a + length b

-- After one step, we should have one fewer card in the deck (unless we
-- started with an empty deck, in which case we should still have an empty
-- deck)
prop_step_oneFewer :: Deck -> Bool
prop_step_oneFewer deck =
    length (fst (step (deck, []))) == newLength deck
    where
    newLength [] = 0
    newLength d = (length d) - 1

-- Take the first element of a list that satisfies a predicate.
first :: (a -> Bool) -> [a] -> a
first p = head . dropWhile (not . p)

-- Perform the shuffle on a deck.
shuffle :: Deck -> Deck
shuffle d = snd . first (null . fst) $ iterate step (d, [])

-- Shuffling a deck should return another deck with the same number of
-- cards
prop_shuffle_sameLength :: Deck -> Bool
prop_shuffle_sameLength deck = length (shuffle deck) == length deck

-- Shuffling a deck should move the top card to the bottom
prop_shuffle_topToBottom :: Deck -> Bool
prop_shuffle_topToBottom [] = True
prop_shuffle_topToBottom deck@(topCard:_) =
    last (shuffle deck) == topCard

-- Get the number of times you have to apply f to z to get z again.
order :: Eq a => (a -> a) -> a -> Int
order f z = (+1) . length . takeWhile (/= z) . drop 1 $ iterate f z

-- Evaluate the graph of a function
graph :: (a -> b) -> [a] -> [(a, b)]
graph f = map (\x -> (x, f x))

-- A naive first attempt at the target function.
f1 :: Int -> Int
f1 = order shuffle . makeDeck

-- Trace performing shuffles.
trace :: Int -> Int -> IO ()
trace max' = mapM_ print . take max' . iterate shuffle . makeDeck

-- If we treat `shuffle` as a permutation in Sn, taking the initial position of
-- a card in a deck size n to the position after one shuffle, we can hopefully
-- glean the order more cleverly by decomposing to disjoint cycles.
newtype Cycle = Cycle [Int]
    deriving (Eq, Ord)

rotate :: [a] -> [a]
rotate (x:xs) = xs ++ [x]
rotate [] = []

-- Pass an infinte cycling list in; rotate until the smallest element is at
-- the front; construct a Cycle. Smart constructor.
makeCycle :: [Int] -> Cycle
makeCycle (x:xs) =
    let ys    = x : takeWhile (/= x) xs
        h     = minimum ys
        elems = first (\(y:_) -> y == h) (iterate rotate ys)
    in  Cycle elems
makeCycle [] = Cycle []

instance Show Cycle where
    show (Cycle xs) = "(" ++ join " " (map show xs) ++ ")"
        where
        join glue (y:ys) = y ++ (concatMap (glue ++) ys)
        join _ [] = ""

cycleLength :: Cycle -> Int
cycleLength (Cycle n) = length n

-- Calculate the shuffle permutation for a deck of size n.
perm :: Int -> [(Int, Int)]
perm n =
    let initial = makeDeck n
        result = shuffle initial
        initial' = unCardAll initial
        result' = unCardAll result
    in catMaybes $ map (\x ->
        fmap (\idx -> (x, idx + 1)) $ elemIndex x result') initial'

-- perm, but faster.
perm' :: Int -> [(Int, Int)]
perm' n = go 0 (unCardAll . shuffle . makeDeck $ n)
    where go m (x:xs) = (x, m+1) : go (m+1) xs
          go _ [] = []

prop_perm_perm'_identical :: Int -> Bool
prop_perm_perm'_identical x = sort (perm x) == sort (perm' x)

-- Take a permutation as [(Int, Int)] and represent it as a product of disjoint
-- cycles.
decompose :: [(Int, Int)] -> [Cycle]
decompose xs = foldl f [] $ take (length xs) (iterate rotate xs)
    where
    f acc ys =
        let cyc = extractCycle ys
        in if cyc `elem` acc
            then acc
            else cyc : acc

-- Extract a single cycle from a permutation [(Int, Int)].
extractCycle :: [(Int, Int)] -> Cycle
extractCycle xs@(h:_) = makeCycle $ go h xs
    where
    go (y, pos) ys = case lookup pos ys of
        Just z -> y : go (pos, z) ys
        Nothing -> cycle [y, pos]
extractCycle [] = makeCycle []

-- Find the lowest common multiple of a list.
lcm' :: [Int] -> Int
lcm' = foldl lcm 1

-- 2nd attempt.
f2 :: Int -> Int
f2 = lcm' . map cycleLength . decompose . perm'

prop_f1_f2_identical :: Int -> Bool
prop_f1_f2_identical x = f1 x == f2 x

-- Shrink the size of arbitrary things. This stops the test cases from running
-- too long.
sensible :: Testable a => a -> Property
sensible = mapSize (floor . logBase (2 :: Double) . fromIntegral)

main :: IO ()
main = do
    args <- getArgs
    case args of
        x:_ -> do
            print . f2 . read $ x
        _   -> do
            -- check all of our properties, while shrinking the sizes
            let qc :: Testable a => a -> String -> IO ()
                qc prop msg =
                    putStrLn ("Testing: " ++ msg) >> quickCheck (sensible prop)
            qc prop_step_sameLength "prop_step_sameLength"
            qc prop_step_oneFewer "prop_step_oneFewer"
            qc prop_shuffle_sameLength "prop_shuffle_sameLength"
            qc prop_shuffle_topToBottom "prop_shuffle_topToBottom"
            qc prop_perm_perm'_identical "prop_perm_perm'_identical"
            qc prop_f1_f2_identical "prop_f1_f2_identical"
