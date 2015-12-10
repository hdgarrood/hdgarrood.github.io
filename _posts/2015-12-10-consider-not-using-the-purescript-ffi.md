---
layout: post
title: A plea; consider not using the PureScript FFI
---

After encounting 3 separate sets of bindings to node's `child_process` API (one
of which is, admittedly, my fault), I felt I needed to write this. I don't mean
to single out anybody; `child_process` is just a particularly unfortunate
example. It's certainly not the only one.

So. I know the FFI is easy to use, and very attractive for certain problems,
particularly when it's a hard to work out how to give things types in
PureScript. But when you feel like you want to use it, I would like to ask you
to consider the following, if you have the time:

### 1. Is there a PureScript library for what I want to do already?
   
If the answer is "yes", but it doesn't quite work for your use case, then
consider opening an issue or sending a pull request. There is a good chance
that this will result in improvements to the library that everyone can then
benefit from.

For example, if you just want to persuade the type checker that a value of some
type is actually some other type, there is a library for that:
[unsafe-coerce](http://pursuit.purescript.org/packages/purescript-unsafe-coerce).

### 2. Could I write this without using the FFI?

If it is possible to write the code you want just in PureScript, without using
the FFI, consider doing so. You may well find that avoiding the FFI means that
you end up with a better architecture. My impression is that, in languages like
JavaScript, the temptation is just too great to mix lots of separate concepts
together and throw mutability or other effects in everywhere, and that the
antidote is strong, static, expressive types (just like PureScript has).

Of course, if you're already using PureScript, I probably don't need to
convince you of this. ;)

For example, you may well find that porting a JavaScript library to PureScript
gives a better result than trying to write bindings to it via the FFI. I
certainly found this to be the case when I wrote
[ansi](http://pursuit.purescript.org/packages/purescript-ansi).

### 3. Could I create a PureScript library for this?

Admittedly, this option is particularly time-consuming. But it does allow you
to leverage the community; I think we're best equipped to design good APIs when
we have different people, with different perspectives or use cases, working
together. When you create a library, you also create a place which enables this
kind of discussion.

### But why?

In addition to what I've already written, there are a few reasons:

Perhaps most importantly, it's safer. It's easy to say, "oh, it's only 5 lines,
what can possibly go wrong?" The answer is: lots. While I was porting Pulp to
PureScript, the largest time sink by far was me making mistakes such as:

* using the wrong number of arguments,
* forgetting to curry,
* forgetting that I need to return a nullary function for `Eff`,
* calling callbacks at the wrong time.

These things, of course, result in runtime errors which are no fun to debug. By
segregating FFI code into libraries, these problems only have to be endured
once.

It means that we eventually fill gaps in the PureScript ecosystem. PureScript
is, of course, still quite young, and has the ecosystem has lots of gaps. After
we've filled them, we'll have more time to address more interesting problems.

Finally, it means that it's easier to use alternative backends. I know of two
in development: there's [pure11](https://github.com/pure11/pure11), which
compiles PureScript to C++11, as well as
[truffled-purescript](https://github.com/slamdata/truffled-purescript), which
uses Truffle to allow PureScript code to run on the JVM. The more use of the
FFI there is across the ecosystem, the harder it is to use other backends.
