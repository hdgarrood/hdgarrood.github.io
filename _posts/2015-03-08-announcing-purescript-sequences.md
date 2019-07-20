---
layout: post
title: Announcing purescript-sequences
---

* <https://github.com/hdgarrood/purescript-sequences>

In many mainstream programming languages, the data structures that you are
provided with are mutable by default. In particular, this is the case with
arrays in JavaScript.

This is arguably quite unfortunate: immutability can offer some very
significant benefits. For example, consider the following (JavaScript) code:

```javascript
var s1 = getDataset1()
var s2 = getDataset2()
var allData = s1.concat(s2)
```

Assume that `getDataset1` and `getDataset2` each returns an array &mdash; so we
are getting two separate data sets and concatenating them.

Now suppose that later on in the code, I want to modify `s1`; let's say, I want
to take `s1[0]` and increment it: `s1[0] += 1`. Should `allData` change in the
same way? Hopefully not; it seems this would lead to horrible bugs which would
be a nightmare to track down.

So we want to ensure that changes in `s1` are not reflected in `allData`.
Sadly, the only way we can do this in the presence of mutability is to copy
the whole array &mdash; despite the fact that, in many cases, we don't even
need two copies of the data, nor do we need to spend the extra time copying
them.

In short, mutability puts us in a bind where we often have no choice but to
waste our computer's valuable memory and time in order to protect us from
ourselves.

In PureScript, however, values are immutable by default. So we should be
able to sidestep this problem. Taking our example from above, there should be
no need to defensively copy everything; our `concat` operation can _reuse_
parts of its arguments in order to decrease the amount of work it has to do, as
well as the amount of memory your program needs to use.

Currently, most PureScript code uses the `Prim.Array` type, which is a normal
JavaScript array. This is often very useful when you want to do interop with
JavaScript libraries. But it prevents us from claiming the benefits of
immutability.

[purescript-sequences](https://github.com/hdgarrood/purescript-sequences) is an
attempt to reap these benefits. It implements a general-purpose sequence data
type, `Seq`. The API is similar to that of an array, but the internal
arrangement of data is very different. By leveraging immutability, it is able
to push a new element on to either end or remove an element from either end in
constant time, and concatenate two sequences in logarithmic time &mdash;
compare this to the corresponding JavaScript array operations, which are both
linear time.

I've just released version 0.1.0, which I believe is ready for public
consumption. I've dogfooded it too; my [multiplayer pacman game](https://mpac.herokuapp.com/)
([source](https://github.com/hdgarrood/multipac)) now uses it internally.

I've also put together a [benchmark](benchmarks/), which shows the time taken
to cons a certain number of elements onto sequences and arrays. As expected,
the sequence results look linear: each `cons` is O(1), so we expect that doing
n `cons` operations is O(n). The array results look quadratic, which is again
what we expect: for arrays, each `cons` is O(n), so we expect n `cons`
operations to be O(n<sup>2</sup>).

However, `Seq` appears to be slower than native arrays up to some crossover
point. On my machine, it's around 6,000 elements. So this library probably
won't give you a speed boost in all cases. It also probably won't be worth
using if you need to pass arrays back and forth between JavaScript libraries
and PureScript code, as converting between the two types is O(n).

Anyway, enjoy! Let me know if it works for you (or even, if it doesn't).
