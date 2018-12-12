---
layout: post
title: Why "If you understand X, you understand monads" is misguided
---

*For X = some specific example of a monad.*

Oops, it's yet another ill-advised post on monad pedagogy! I'm actually
(mostly) joking; I feel that what I'm about to say needs saying because I don't
think I've seen it written down before. I also did a quick google and didn't
find anything. If this does actually exist already please do let me know.

Before I start I should also clarify that I mean "monad" in the Haskell sense,
i.e. a type constructor for which you can define a `bind` and a `pure` function
which, together, satisfy the laws. I don't mean "monad" in the category theory
sense.

So something I see fairly often is "if you can understand promises/IO/[some
other specific example of a monad], you can understand monads." My impression
is that what is usually meant is "if you are capable of writing programs which
make use of promises, you are 99% of the way to understanding monads, because
promises are an example of a monad," which I vehemently disagree with.

(I do actually agree with the original statement, but for a different reason:
I believe that pretty much anyone who is capable of computer programming is
capable of understanding monads. Yes, I'm 100% serious. That's maybe a topic
for later, though).

The reason I don't believe the previously mentioned statement is true is that
the *generalising step*, the cognitive step that takes you from being able to
reason about promises/IO/whatever to being able to reason about a completely
arbitrary type, where the only thing you know about it is that it is a monad,
is really quite a big step. In fact, it is *the* step. I can't imagine how we
might define the concept of "understanding monads" in any other way than
"having been able to make this step".

Further, it's possible to write programs that make use of promises without
having (yet) made this step. I imagine this is the case for quite a large
proportion of web developers right now, even.

To better illustrate what I mean, I'm going to take an example from
mathematics, specifically groups, which come from abstract algebra.

So I think the vast majority of people emerge from school understanding a few
things about addition and subtraction: first, that the sum of any two integers
is itself an integer; second, that you don't need to bother with brackets in an
expression like 2 + 13 + 7 + 103 + 3, because any bracketing of it will always
yield the same result; third, that adding 0 to any number doesn't change it;
and fourth, that subtracting anything from itself yields 0.

These four properties (or rather, more general versions of them) are the
defining characteristics, or *axioms*, for a group; they are analogous to the
monad laws for monads. But understanding and being able to apply these facts in
the case of integers certainly does not mean you understand groups. For
example, students of group theory will generally encounter the theorem of
uniqueness of inverses very early on. Is it reasonable to assume that the only
prerequisite for being able to follow the proof of this theorem is
understanding those properties of addition? Of course not; you need to be
comfortable with working in a more abstract setting to be able to see what's
really going on here.
