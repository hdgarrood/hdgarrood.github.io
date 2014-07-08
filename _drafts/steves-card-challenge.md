---
layout: post
title: "Steve's Shuffle"
---

A colleague of mine severely [nerd-sniped][] me at work recently with this
rather innocuous-sounding challenge. I think it's a rather lovely little
exercise and decided to write up my solution.

This post is designed to be read as a programming exercise. It will guide you
along as it helps you break the problem into parts. If you want, you can skip
to the end and read all the solutions&mdash;but that would be cheating.

The problem is as follows:

> Given the following shuffling technique:
>
>   1. Take the top card off the top of the deck and place it in a new pile.
>   2. Take the next card off the top of the deck, and put it at the bottom of
>      the deck.
>   3. Repeat these two steps (placing all cards in the same new pile) until
>      the original deck is all gone and the new pile has all the cards in it.
>
> How many shuffles does it take until the deck is in the same order as when
> you started?

We're going to use Haskell, because this is all about *functions* (in the
mathematical sense), and so Haskell, being a *functional programming language*,
is especially well suited to the job.

Let's start with this:

    -- A Card is like an Int, except that the type checker won't let you put an
    -- Int where a Card should go, and vice versa.
    newtype Card = Card Int
        deriving (Show, Eq, Ord)

    -- A Deck is just a list of Cards. We consider the first element of the
    -- list to be the top of the deck.
    type Deck = [Card]

    -- Create a deck with 'n' cards, where 1 is at the front.
    deck :: Int -> Deck
    deck n = map Card [1..n]

To make it easy for us, we should try to split the problem into small pieces
first. Therefore the first job we have is to define a function, `step`, that
performs steps 1 and 2 of our shuffling technique. What type should this
function be? It needs to take a deck and a pile, and return a new deck and
a new pile. So let's go with `(Deck, Deck) -> (Deck, Deck)`.

[nerd-sniped]: https://xkcd.com/356/
