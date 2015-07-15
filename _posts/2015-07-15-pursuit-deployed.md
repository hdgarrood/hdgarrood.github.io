---
title: Pursuit 2 has been deployed!
layout: post
---

* Pursuit: <http://pursuit.purescript.org>

The next iteration of the PureScript code search tool, Pursuit, is quite a
large change from previous versions. It now acts as a central place for
documentation of PureScript packages, as well as allowing you to search through
PureScript code. In particular:

* All types in documentation are links pointing to the documentation for that
  type. For example, if you are looking at the [Signal][] data type from
  purescript-signal, and see its Semigroup instance listed, but you forget what
  that means, you can simply click on it to be taken to the [Semigroup][]
  definition.
* Pursuit now integrates [Hoogle][] for code search, meaning that the name
  search is significantly better, and also meaning that you can search by type!
  For example, try searching for [s (t a) -> t (s a)](http://pursuit.purescript.org/search?q=s+%28t+a%29+-%3E+t+%28s+a%29).
* Each package now has its own homepage, showing where it can be found on
  GitHub, and displaying its README and a list of the modules it contains.
* Unlike GitHub markdown documentation, Pursuit will only display released
  versions of packages, so that you never are accidentally looking at the
  documentation for the master branch.

[The package uploading guide][] explains how to upload documentation for your
own packages.

You can access Pursuit at <http://pursuit.purescript.org>. (The old one is now
at <http://old-pursuit.purescript.org>). For any comments, suggestions, or bug
reports please report issues [on
GitHub](https://github.com/purescript/pursuit/issues) or let us know on
\#purescript IRC on Freenode.

[Signal]: http://pursuit.purescript.org/packages/purescript-signal/4.1.0/docs/Signal#d:Signal
[Semigroup]: http://pursuit.purescript.org/packages/purescript-prelude/0.1.0/docs/Prelude#d:Semigroup
[Hoogle]: http://www.haskell.org/hoogle
[the package uploading guide]: http://pursuit.purescript.org/help#submitting-packages
