---
layout: post
title: Thoughts on PureScript package management in 2019
---

It's been around 3 and a half years since I wrote [this post about why
PureScript uses Bower and not npm](../purescript-why-bower). To recap, the
reason we use Bower rather than npm by default for PureScript is that npm
handles dependency incompatibilities in a way which does not work with
PureScript; specifically, allowing multiple different versions of a dependency
to exist in your dependency tree is not supported, because it's extremely
difficult to achieve safely.

While that post is still accurate, a few things have changed in the meantime,
and there are a few frequently asked questions I think are worth clarifying.

Before I begin I'd like to mention that this is just my personal view, which is
not necessarily shared by other maintainers.

So, some frequently asked questions:

### Isn't it more urgent to move away from Bower now that it is deprecated?

I don't think so. The fact that Bower now prints a deprecation message when you
install it does hurt UX a bit, which is unfortunate, but not much has really
changed with Bower's development process. It is not getting new features, but
it is still getting bug fixes and security updates, and it still does what it's
supposed to (well, apart from the fact that its solver is still very basic and
often fails to find install plans even where they do exist, but that was always
the case).

To be clear, there are good reasons to move away from Bower, but in my mind,
the fact that it prints a deprecation message on installation is not
particularly high on the list.

### What about yarn and flat mode?

Yarn is an alternative package manager for JavaScript which offers a `--flat`
command line option to allow you to specify that you only want one version of
any given dependency to exist throughout your install tree. At first, this
seems like it provides precisely what we're missing from npm! Unfortunately,
it's not quite flexible enough to fill the gap, since lots of npm packages
_rely_ on npm's dependency nesting in order to be able to install. Therefore,
it needs to be possible to specify that some packages be installed 'flat', and
others may use nesting. Specifically, what we want by default is that
PureScript packages should be installed flat whereas JavaScript packages should
be installed as usual, that is, with nesting if necessary.

Furthermore, requesting that at most one version of any PureScript dependency
may exist in a dependency tree really needs to be something the package
_author_ is responsible for specifying, rather than the package _consumer_.
I think that leaving it up to the consumer would result in a poor UX, because
it would be easy to forget to do and would probably result in cryptic errors
when you did (possibly "module not found" or "duplicate module" errors).

There is an [npm rfc for singleton
packages](https://github.com/npm/rfcs/pull/23) which addresses both of these
issues, but it has not yet been accepted or implemented.

### What about package sets and Spago?

Most package managers, including Bower, work by having package authors specify
version ranges, or bounds, on each of their dependencies. When you ask your
package manager to install those dependencies, it tries to find an _install
plan_, that is, a specific version for each of your dependencies (including
your transitive dependencies) such that for each dependency, the version
which has been selected satisfies all of the constraints imposed on that
dependency, for everything else in the install plan which depends on it. For
example, if:

- you depend on packages A and B,
- B depends on A as well,
- you specify a version range of `>=1.0.0 <3.0.0` on A,
- the version of B you want specifies a version range of `>=1.0.0 <2.0.0` on A,

then a solver might end up selecting, say, version 1.1.0 of A. Note that
even if a version 2.0.0 of A is available, we can't select it because we need
to satisfy our own constraints on the version of A we select _as well as_ B's.

In the meantime since I wrote that Bower post, an alternative approach called
'package sets' has appeared, inspired by Haskell's [Stackage][]. In the
package-set approach, a brave group of people volunteer as _package set
curators_, and they assemble a collection of libraries at fixed versions such
that all of the libraries are known to work together. Package sets themselves
also have versions: you generally will depend on a particular package set at a
particular version, which is immutable. When new versions of libraries in the
set are released, the curators check that everything still works properly after
updating, and if it does, a new set is constructed with updated versions of
everything, and this set can be tagged and released as a new version.

The general idea is that by performing curation, the work of getting libraries
to build together can be done once (by the curators) and then reused by
everyone else. By comparison, when libraries release breaking changes in a
bounds-and-solving-based ecosystem without package sets, it can sometimes be
difficult to find install plans (especially if you don't have a very
sophisticated version solver, which we don't); package sets provide a way of
reusing the work done to find successful install plans.

So what about package sets? I think they're great and you should totally use
them! [Fabrizio][] and [Justin][] have been doing some really fantastic work on
the [package-sets][] repository, which provides PureScript package sets, and
[Spago][], a package manager and build tool which makes use of package sets to
make it easier to get all of your dependencies installed and working together.

Seriously, if you use PureScript and you haven't tried Spago out yet, drop
everything and do it right now.

### Package sets versus bounds and solving

Note that the two approaches are _not_ incompatible with each other. Take for
example the Haskell ecosystem, which makes good use of both approaches. In
particular, I suspect that the fact that Haskell libraries (mostly) have
version bounds makes the package set curators' job quite a bit easier, since it
makes the task of letting library authors know that their libraries need
updating (and why) much more easily automatable; see the [Stackage issue
tracker][] for some examples. Also, I'm often very grateful that library
authors include version bounds in their Haskell libraries, because occasionally
you do need to make tweaks to your package set, e.g. to use the latest version
of a particular library because you need a particular feature or fix. When you
do this, you may find that your dependencies no longer compile together, and
your package set can no longer help you resolve this. It's in this situation
that version bounds (and a solver) can be really helpful.

I think the ideal situation for package management is that you can choose where
to situate yourself on the spectrum where you have no package sets and just
version solving at one end, and no solving and just package sets on the other.
Currently in PureScript you have to choose between one extreme or the other.

### Some other things I'd like to see in PureScript package management in the future

Two of the most important properties you want in package registries are
_availability_ and _integrity_. By 'availability' I mean being able to get
things when you ask for them; a package registry which was often down for
maintenance would not score highly on availability. By 'integrity' I mean being
sure that nobody has tampered with something when you download it.

One of my least favourite things about the current state of PureScript package
management is that it is all based on tags in git repositories, which are
pretty much the worst possible option on both fronts.  If a maintainer of any
package you depend on decides one day that they can't be bothered any more and
deletes the repository, your build breaks. (Hopefully someone else has a cached
copy.) Additionally, a well-meaning maintainer could mutate a tag in their
repository to point at a different commit, meaning that different people get
different code depending on when they performed the install.

I would very much like PureScript to start using a proper package registry at
some point in order to address these issues. It would be great to be able to
use the npm registry, but that might be awkward without the singleton packages
RFC (unless we want to hack together a nonstandard client or something). Other
interesting possibilities are [entropic][] and [IPFS][].

### Closing thoughts

Realistically my hands are already more than full with the compiler and core
libraries, so I'm very unlikely to be able to dedicate anywhere near the
sufficient amount of time to make progress on the above issues.

[Stackage]: https://stackage.org
[Fabrizio]: https://github.com/f-f
[Justin]: https://github.com/justinwoo
[package-sets]: https://github.com/purescript/package-sets
[Spago]: https://github.com/spacchetti/spago
[IPFS]: https://ipfs.io
[entropic]: https://github.com/entropic-dev/entropic
[Stackage issue tracker]: https://github.com/commercialhaskell/stackage/issues
