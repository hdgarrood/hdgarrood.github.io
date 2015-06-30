---
title: 'GSOC progress update #1: First steps'
layout: post
---

I'm going keeping a blog of progress on my Google Summer of Code project this
summer, which is various enhancements to the PureScript code search tool
Pursuit.

I have managed to get quite a bit done already, since I had been contributing
to PureScript (and Pursuit in particular) for a while before I even applied for
Google Summer of Code. So, for the first entry, I'm just going to go over
what's been done so far:

* A basic filesystem-based database for storing packages. At the moment, the
  only queries that need to be performed are "what versions are available for
  this package, if any" and "give me the documentation for this version of this
  package", and both of these are easily answerable with the current structure:
  one directory per package, and each package directory contains one file for
  each version of the package. The files themselves contain a JSON-serialized
  `Package GithubUser` (see [the relevant code inside the compiler][]), which
  has all the information needed to render a package homepage, documentation,
  and also Hoogle input files.

* HTML documentation rendering, adapted from psc-pages and moved into Pursuit
  itself. I also made a few minor changes to the rendering &mdash; for example, 
  data constructors are now displayed separately from instances, and instances
  are grouped under the relevant types or type classes.

* A pull request sent to the compiler itself, to allow collection of fixity
  information for operators (that is, left- right-, or no associativity, and
  precedence). This information can then be used in the generated HTML
  documentation.

* A mechanism for uploading packages. I wanted to make this as streamlined as
  possible. Taking Haskell for example, you would run `cabal sdist` to create a
  source distribution as a local file, and then you would visit the upload page
  on Hackage in a browser, find your source distribution again using a file
  browser, and upload it. This is a little awkward and I think it should be
  possible to do better.

  Given that I didn't want to manage a users and passwords inside Pursuit, the
  approach I decided on was to allow unauthenticated uploads, but not to
  actually publish packages until they have been verified by an authenticated
  GitHub user. Currently the way this works is:

  1. A library author runs `psc-publish` inside their PureScript package
     directory, which produces a JSON-serialized version of the package to be
     used on Pursuit.
  2. Some command line tool POSTs that JSON to the Pursuit server (no such tool
     exists just yet).
  3. The Pursuit server generates a random verification URL, and replies with
     that URL.
  4. The user visits that URL in their browser, and is prompted to log in via
     GitHub (with OAuth).
  5. After the user is authenticated, the package is considered verified, the
     GitHub user is recorded as being the package uploader, and the HTML
     documentation for that package becomes visible.

* Some kind of caching mechanism. The HTML documentation will very rarely
  change, so we don't want to do a ton of JSON parsing and documentation
  generating every time a page is requested. At the moment we have a very basic
  system which the saves expensive bits of HTML to disk, and deletes them
  whenever they expire.

[the relevant code inside the compiler]: https://github.com/purescript/purescript/blob/cdd856c1c568094f5bc0a1bb913d050df828e2d0/src/Language/PureScript/Docs/Types.hs#L38-L48

