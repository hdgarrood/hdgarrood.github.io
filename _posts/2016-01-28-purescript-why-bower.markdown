---
layout: post
title: Why the PureScript community uses Bower
---

*(or, perhaps more appropriately, "Why the PureScript community does not use
npm")*

Lots of people dislike having to use Bower as the PureScript package manager,
for various reasons. It certainly has problems, but I do think it's the best
choice currently available, so I am writing this post, which explains why.

Before I start, a quick notice: if Bower (or any other part of the standard
PureScript toolchain) is causing you problems, we would like to know! The
\#purescript IRC channel on freenode is a good place to ask, as is the [mailing
list](https://groups.google.com/forum/#!forum/purescript).

## Handling dependency conflicts

The main reason PureScript does not use npm is to do with the way npm deals
with *dependency conflicts*.

For example, suppose I am writing a package which depends on `purescript-maps`
with bounds `>= 2.0.0 < 3.0.0`. Suppose we now want to depend on some other
package; let's call it `purescript-foo`. The `purescript-foo` package happens
to declare different version bounds for `purescript-maps`: it uses `>= 1.0.0 <
2.0.0`. So if we wrote out our dependency tree, it might look like this:

    my-package
      purescript-maps: >= 2.0.0 < 3.0.0
      purescript-foo: *
        purescript-maps: >= 1.0.0 < 2.0.0

Unfortunately, there is no version of `purescript-maps` which can satisfy both
of these constraints. So what does the package manager do here?

## Nesting dependencies

npm's solution to this problem is to nest the dependencies. The files end up
looking a bit like this on the disk:

    my-package
      node_modules
        purescript-maps (at 2.0.0)
        purescript-foo
          node_modules
            purescript-maps (at 1.0.0)

Unfortunately, this doesn't work well with PureScript code. Suppose that
`purescript-foo` exports a function like this:

    doSomething :: Map Foo Bar -> Baz

So, you call `doSomething`, providing a `Map Foo Bar`, which you constructed
with your copy of `purescript-maps`. This gives you a runtime error!

One of the main benefits of PureScript is supposed to be code that you can have
confidence in; runtime errors should turn into compile-time ones. So we can't
have this.

I'm going to delve into the technical details of how this happens and how we
might stop this from happening now, but in case you don't want to read all
that, the short answer is: Bower solves this problem by having "flat"
dependencies. No nesting occurs; instead, if there are conflicts, Bower will
ask you how to resolve them. That is, it will ask you to choose one particular
version, even though it will violate the constraints declared by one or more of
the other packages.

In most cases, though, it's better to loosen one or more of your constraints,
or your dependencies' constraints, so that an install plan can be found. This
will sometimes require changes to your code and/or your depenencies' code.

Of course, this situation is not ideal either, but it's much better than
runtime errors. Additionally, there is a lot we could do to reduce the
likelihood of such dependency conflicts happening (and I might write about this
later).

## Technical details: what happened?

That runtime error happened because `purescript-foo` is expecting the `Map`
argument to `doSomething` to have been constructed by the same copy of
`purescript-maps` as the one it has installed. Currently, pattern matching on
sum types in PureScript is based on `instanceof` checks, which effectively
means that passing values between different versions of dependencies like this
is not safe &mdash; a value constructed by `purescript-maps@1.0.0` will not be
considered to be an `instanceof` the `Map` type in `purescript-maps@2.0.0`.

There are a few things we could do to alleviate this issue, and I'm going to
discuss a few of them now, but my current view is that they all end up
introducing worse problems, and so I think we should stick to flat dependencies
for now.

### Distinguishing versions in the type checker

One solution could be to allow multiple versions of a particular library to be
installed, but distinguish them in the type checker, so, for example,
`purescript-maps@1.0.0:Map` would be a separate type from
`purescript-maps@2.0.0:Map`. It's not clear whether this is a good idea,
though.  While it goes some way towards alleviating problems of "dependency
hell" by reducing the likelihood of dependency conflicts, it introduces new
problems:

* It's still possible to reach a situation where you need a 1.0.0 `Map` but you
  only have a 2.0.0 `Map`. For maps, this situation is not too dire, as you
  might be able to convert between them. For other data types, you might be
  completely stuck. And even if you can convert between them, this is *at
  least* an O(n) cost every time you do.
* The size of your code could increase hugely, especially for larger projects.

*Note that these ideas came from Evan Czaplicki, the creator of Elm, and not
me. See also the relevant [elm-package issue][].*

### Private dependencies

An alternative approach could be to allow "private dependencies". For example,
let's suppose now that some other library, `purescript-bar`, depends on
`purescript-maps`, but only internally: no part of the dependency on
`purescript-maps` "leaks" out into `purescript-bar`'s API. Now, there should be
no risk of such a runtime error occurring, right?

Unfortunately, it defining what a "private" dependency actually is seems quite
thorny: consider these slides from a talk by Andres Löh and Duncan Coutts from
Well-Typed, which broadly discusses the same issues in the context of Haskell:

![Encapsulations are subtle](/assets/img/encapsulations-are-subtle.png)

Further, if there are two copies of the same *function* from two different
versions of the same library, and these functions happen to have the same type
but have slightly different behaviour, it's probably not going to be obvious
which one is actually being used in some situations. I worry that this could
lead to bugs which would be incredibly difficult to diagnose.



## Some additional notes/resources

* This post is based on npm >= 3. npm <= 2 behaves slightly differently:
  dependencies are *always* nested, even if there are no conflicts, which means
  you're even more susceptible to these kinds of problems. See also [npm's
  documentation on how it nests
  dependencies](https://docs.npmjs.com/files/folders).
* [The full slides from that Well-Typed talk][] which I mentioned earlier.


[smart constructors]: https://leanpub.com/purescript/read#leanpub-auto-smart-constructors
[this PR on purescript-free]: https://github.com/purescript/purescript-free/pull/37
[elm-package issue]: https://github.com/elm-lang/elm-package/issues/89#issuecomment-69499678
[The full slides from that Well-Typed talk]: https://wiki.haskell.org/wikiupload/b/b4/HIW2011-Talk-Loeh.pdf
[Bryan O'Sullivan's post]: https://plus.google.com/103469090998089605155/posts/HdC6oCy8RWW
