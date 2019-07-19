---
title: Different kinds of integer division
mathjax: true
layout: post
---

Suppose we wish to define an integer division operation, that is, a division
operation which accepts two integers and returns another integer. This blog
post will address two issues:

1. how to decide whether a particular operation can sensibly be described as
   'integer division', and
2. why there are a few different options, and what the differences between them
   are.

Before we start, some vocabulary. If we write $a / b$, then $a$ is called the
_dividend_, and $b$ is called the _divisor_.

## What is a sensible notion of 'integer division'?

First, I want to argue that when we define a form of integer division, we
really ought to consider the result to be a _pair_ of integers; the first is
called the _quotient_, and it represents the number of times the divisor 'goes
into' the dividend, and the second is called the _remainder_, and it represents
what is left over after taking away that multiple of the divisor.

We can formalise this: suppose we have some form of integer division, and we
use it to divide $a$ by $b$, obtaining a quotient $q$ and a remainder $r$.
The first thing we need to do in order to ensure that we have an operation
which can sensibly be described as 'division' is to require that $a=qb+r$. This
requirement sort of corresponds to saying that "the number of times $b$ goes
into $a$ is $q$, and the remainder is $r$". For example, $5$ goes into $13$
twice with remainder $3$, so if we choose $a=13$ and $b=5$, we get $q=2$ and
$r=3$.

After adding this requirement, $q$ and $r$ become closely related. In
particular, once we've chosen what we want one to be, there is only one choice
for the other.

Note that if we fix $a$ and $b$, and we have a pair of integers $q$ and $r$
such that $a=qb+r$, then we can always find a different choice for $q$ and $r$
which also satisfy our requirement. For example, we can perform the
substitutions $q \to q-1$ and $r \to r+b$, since $(q-1)b + (r+b) = qb - b + r +
b = qb + r = a$. Similarly, we can substitute $q \to q+1$, and $r \to r-b$.
Therefore, when we are defining an integer division operation, since we have
many different options for $q$ and $r$, we must decide which one we are going
to choose. In fact, at this point, we have an infinite number of choices for
$q$ and $r$ given $a$ and $b$; this suggests that we need to narrow down our
options a little more by adding more constraints.

There is another additional constraint we can add now to ensure that we have
something that can sensibly be described as 'integer division': we can require
that $q$ is close to the result of exact division (which usually will not be an
integer). More specifically, we can require that the difference between $q$ and
the exact result $a/b$ is less than $1$. We can express this in symbols: $|q -
a/b| < 1$.

One consequence of this requirement is that if $b$ goes exactly into $a$, we
must take $q$ to be the exact result of the division, and $r$ to be $0$. For
example, if we have $a=-8$ and $b=2$, then the result of exact division is
$-4$, so in this case we must take $q=-4$ and $r=0$.

There is one more constraint we can add, due to the fact that pretty much
everyone agrees on what the result of integer division should be when $a$ and
$b$ are both positive. In this case, we should take $r$ such that $0 \leq r <
b$, and we should take $q$ to be the largest integer satisfying $qb \leq a$.
Equivalently, we obtain $q$ by taking the exact result of dividing $a$ by $b$
and rounding down to the nearest integer. Going back to one of our previous
examples, if $a=13$ and $b=5$, the largest $q$ we can take is $2$, because $5
\times 2=10$, and $10$ is the largest multiple of $5$ which is less than $13$.

So we end up with the following definition of a 'sensible' integer division
operation. It is a function which takes a dividend $a$ and a nonzero divisor
$b$ as inputs, and returns a quotient $q$ and a remainder $r$, subject to the
following constraints:

1. $a = qb + r$,
2. $\lvert q - a/b \rvert < 1$,
3. If $a,b > 0$, then $q = \max \\{ t \in \mathbb{Z} : bt \leq a \\}$.

## So what are the options?

As we have seen, there is only one option which satisfies all three of these
constraints if both of $a$ and $b$ are nonnegative, or if $b$ goes into $a$
exactly. However, in the case where either (or both) of $a$ or $b$ are negative
AND when $b$ does not go into $a$ exactly, there are a few different options to
choose from.

One of the most common options taken by programming languages is called
"truncating" division, because we obtain $q$ by taking the exact result of
division and "truncating" (rounding towards zero). This can be implemented
in JavaScript as follows:

```javascript
// Truncating division
function tdiv(x,y) {
  return Math.trunc(x / y);
}
function tmod(x,y) {
  return x % y;
}
```

To give an example, suppose we have $a=-2$ and $b=3$, and we want to divide $a$
by $b$. The exact result of division is $-2/3$; rounding this towards zero, we
obtain $q=0$. Then, we must choose $r=-2$ so that the first constraint is
satisfied.

The other most common option is called "flooring" or "Knuthian" division. It
works by taking the exact result of division and then rounding towards negative
infinity. In JavaScript:

```javascript
// Flooring/Knuthian division
function fdiv(x,y) {
  return Math.floor(x / y);
}
function fmod(x,y) {
  return ((x % y) + y) % y;
}
```

Notice that, if the exact result of division is positive, flooring division is
identical to truncating division. However, if the exact result is negative,
then the results of flooring and truncating division will be slightly
different. For example, consider $a=-2$ and $b=3$ again. We saw that truncating
division produces $q=0$ in this case, but flooring division rounds towards
negative infinity and so we obtain $q=-1$. Now we just need to find what
remainder flooring division gives us in this case. To do this, we can use the
requirement from earlier; we need to choose $r$ so that the equation $-2 = (-1
\times 3) + r$ is satisfied, and of course the only such choice is $r=1$.

Now consider a similar example, where we wish to perform flooring division with
$a=2$ and $b=-3$. In this case, we again obtain $q=-1$. Since $a=2$, we need to
choose $r=-1$ to satisfy our requirement. Notice that flooring division gave us
a positive $r$ before, but this time, it gave us a negative $r$.

In fact, it turns out that, with truncating division, the remainder $r$ always
has the same sign as the dividend $a$, and that with flooring division, the
remainder $r$ always has the same sign as the divisor $b$.

It is arguably a severe drawback of truncating division that the remainder
takes the sign of the dividend; as any number theorist will tell you, when
performing division with a dividend $a$ and a divisor $b$, it makes the most
sense to consider only $|b|$ different possibilities for the remainder. For
example, if we are dividing by $3$, there are precisely $3$ possibilities for
how the remainder can turn out: either the dividend goes exactly and there is
no remainder, or there is a remainder of $1$, or there is a remainder of $2$.

However, with truncating division, the fact that the remainder can be either
positive or negative means that we actually have $2|b| - 1$ possibilities.

Flooring division improves on this situation in that if we fix a divisor $b$,
there are $|b|$ possibilities for the remainder $r$, as we wanted. However, as
we have seen, we don't know whether $r$ will be positive or negative without
knowing what $b$ is.

It is arguably more useful to have a form of division in which the remainder
is always nonnegative. There is in fact a form of division which satisfies
this, and it is called Euclidean division:

```javascript
// Euclidean division
function ediv(x,y) {
  return Math.sign(y) * Math.floor(x / Math.abs(y));
}
function emod(x,y) {
  var yy = Math.abs(y);
  return ((x % yy) + yy) % yy;
}
```

The rounding behaviour of Euclidean division is a little more complex, in
order to accommodate our additional requirement that the remainder should
always be nonnegative. With Euclidean division, the type of rounding depends on
the sign of the divisor. If the divisor is positive, Euclidean division rounds
towards negative infinity. If the divisor is negative, Euclidean division
rounds towards positive infinity.

If this seems a bit overly-complicated, there is another way of
understanding these different kinds of division, by considering the signs of
the dividend and the divisor.

* If both dividend and divisor are positive, then all three definitions
  agree.
* If the dividend is positive and the divisor is negative, then the
  truncating and Euclidean definitions agree.
* If the dividend is negative and the divisor is positive, then the flooring
  and Euclidean definitions agree.
* If both dividend and divisor are negative, then the truncating and
  flooring definitions agree.

You can try it out using the table below.

<style type="text/css">
  table.division {
    margin-left: auto;
    margin-right: auto;
  }
  table.division td {
    line-height: 150%;
  }
</style>

<table class="division">
  <tr>
    <td>Dividend</td>
    <td><input style="width: 5em" type="text" id="input-dividend"></td>
  </tr>
  <tr>
    <td>Divisor</td>
    <td><input style="width: 5em" type="text" id="input-divisor"></td>
  </tr>
  <tr>
    <td>Exact result</td>
    <td><span id="span-exact-result"></span></td>
  </tr>
  <tr>
    <td>Result with truncating division</td>
    <td><span id="span-trunc-result"></span></td>
  </tr>
  <tr>
    <td>Result with flooring division</td>
    <td><span id="span-floor-result"></span></td>
  </tr>
  <tr>
    <td>Result with Euclidean division</td>
    <td><span id="span-euclidean-result"></span></td>
  </tr>
</table>

## Comparison of some programming languages

Java's `/` and `%` operators, when applied to integers, implement truncating
division. The `Math` class provides flooring division via `Math.floorDiv` and
`Math.floorMod`.

Python's and Ruby's `/` and `%` operators both implement flooring division.

Haskell's `div` and `mod` implement flooring division, whereas `quot` and `rem`
implement truncating division.

JavaScript's `%` operator implements remainder with respect to truncating
division, but there isn't a truncating integer division operator to go with it,
probably because JavaScript doesn't really have integers (yet). However, you
can implement one yourself quite easily via `Math.trunc(x/y)`.

The main reason I ended up researching and writing this is because we are
considering changing PureScript's behaviour in an upcoming release. Currently,
the functions `div` and `mod`, when specialised to the builtin `Int` type,
implement truncating division, but in a future release they may implement
Euclidean division.

## Bibliography

Much of the insight in this post comes from the paper

> "The Euclidean definition of the functions div and mod", Raymond T. Boute,
> _ACM Transactions on
Programming Languages and Systems_, Vol 14, No. 2, April 1992, pages 127-144.

In particular, if you're after a more detailed discussion of which form of
integer division is the 'best', this paper is a good place to start.

Wikipedia is also a good resource; see
<https://en.wikipedia.org/wiki/Modulo_operation>.

Guido van Rossum [wrote in 2010 about why integer division in Python
floors](http://python-history.blogspot.co.uk/2010/08/why-pythons-integer-division-floors.html).
His post also offers a possible explanation for why truncating division is
common, despite it not really having many nice mathematical properties.

<script src="division.js"></script>
