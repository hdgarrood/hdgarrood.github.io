---
layout: default
title: Projects
---

# Projects

## Pursuit

Pursuit is one of the contributions to PureScript which I'm most proud of. It
hosts searchable API documentation for PureScript libraries, and it initially
came to life in 2015 as my Google Summer of Code project. It's also had a
number of enhancements since then, from various contributors.

<div class="project-links">
  <a href="https://pursuit.purescript.org">
    <span class="label">Pursuit</span>
  </a>
  <a href="https://github.com/purescript/pursuit">
    <span class="label">Source code on GitHub</span>
  </a>
</div>

## Explorations in abstract algebra in PureScript

While studying maths at university, I often found it quite useful (and fun) to
try implementing concepts from abstract algebra in PureScript. For example,
most group theory courses will involve some work with permutations, and working
with permutations by hand can be quite fiddly and error-prone, so being able to
check your work is quite handy.

One quality of PureScript which makes it a great choice for this kind of
exploration is that its numeric type class hierarchy is explicitly inspired by
abstract algebra: there are type classes for semirings, rings, fields, and
others. However, this hierarchy can be a bit difficult to understand,
especially for people who don't have previous experience of abstract algebra.
In 2017-18, I wrote [a guide to the PureScript numeric
hierarchy](https://a-guide-to-the-purescript-numeric-hierarchy.readthedocs.io/en/latest/introduction.html)
to try to rectify this.

Another one of my projects in this vein was a [Legendre symbol calculator](/legendre-symbol-calculator).  Sometimes when you're doing number theory, it's useful to know whether a number is a square modulo some odd prime _p_ (for reasons I don't completely remember).  The [Legendre symbol](https://en.wikipedia.org/wiki/Legendre_symbol) can tell you whether this is the case, and by making use of some of its properties we can quickly determine whether a number is a square without needing to calculate the squares of every number modulo _p_.

<div class="project-links">
  <a href="https://a-guide-to-the-purescript-numeric-hierarchy.readthedocs.io/en/latest/introduction.html">
    <span class="label">Numeric hierarchy guide</span>
  </a>
  <a href="https://pursuit.purescript.org/packages/purescript-symmetric-groups/0.1.2/docs/Data.SymmetricGroup">
    <span class="label">Symmetric groups library</span>
  </a>
  <a href="https://pursuit.purescript.org/packages/purescript-polynomials/1.0.1/docs/Data.Polynomial">
    <span class="label">Polynomials library</span>
  </a>
  <a href="/legendre-symbol-calculator">
    <span class="label">Legendre symbol calculator</span>
  </a>
</div>

## Games

The idea of being able to create video games was mostly what got me into
programming originally, because I am a walking stereotype. Here are a few of
mine.

The [multiplayer pacman](https://mpac.herokuapp.com) and
[solitaire](/projects/solitaire) games are both written in PureScript. The
multiplayer pacman has a server and a client component, both in PureScript,
with node.js on the backend. King of the Network was my group's entry in the
2013 University of Edinburgh GameDevSoc game jam, and it won! (Mostly by virtue
of being one of the only entries which was actually complete enough to be
playable.) Unfortunately the source code for that one is lost. The [tank
game](/projects/tank-game) is quite special to me because it was my first
JavaScript project; it's from 2012, back when I had just started programming.

<div class="project-links">
  <a href="https://mpac.herokuapp.com">
    <span class="label">Multiplayer pacman</span>
  </a>
  <a href="/projects/solitaire">
    <span class="label">Solitaire</span>
  </a>
  <a href="https://pursuit.purescript.org/packages/purescript-polynomials/1.0.1/docs/Data.Polynomial">
    <span class="label">King of the Network (.exe download)</span>
  </a>
  <a href="/projects/tank-game">
    <span class="label">Tank game</span>
  </a>
</div>
