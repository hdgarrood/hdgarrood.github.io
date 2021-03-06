---
layout: post
title: 'GSOC progress update #2: Extracted libraries & Hoogle'
---

More progress updates on my GSOC project. Things seem to be going well!

## Extracted Haskell libraries

I extracted two Haskell libraries out of what I've been doing recently.

The first, aeson-better-errors, arose from a desire to get better errors out of
aeson parsers for common errors, such as type mismatches, missing object keys,
and so on. In particular, I wanted to know exactly where in the JSON the
problem is, since this makes diagnosing some problems significantly easier; for
example, bugs in serializers, or attempting to parse data using an older
serialization format.

Hopefully an example will make it clear what I mean:

    deeplyNested :: Parse e Int
    deeplyNested = key "a" (nth 3 (key "b" (key "c" asIntegral)))

    λ: printErr $ parse deeplyNested "{\"a\":[null,null,null,{\"b\":{\"c\":null}}]}"
    At the path: ["a"][3]["b"]["c"]
    Type mismatch:
    Expected a value of type number
    Got: null

One of my aims for the tooling around Pursuit, and particularly the process of
submitting your packages, is that it should be as pleasant as possible. We want
people to use it, after all! I created aeson-better-errors as a part of this
aim.

You can find [aeson-better-errors on Hackage][]. I also wrote a
[tutorial/introduction blog post][].

The other library, bower-json, is a little less interesting. It simply provides
a data type and `ToJSON`/`FromJSON` instances for Bower's package manifest file
format (that is, `bower.json` files). You can find [bower-json on Hackage][]
too.

## Changes to the psc-publish tool

`psc-publish` is the tool which looks through all the source code of a
PureScript package, extracts the information required to host it on Pursuit,
and dumps that information as JSON. Previously, this included rendering code;
that is, taking parts of a PureScript AST and converting them into an
intermediate format called `RenderedCode`, suitable for generating plain text
or highlighted HTML from. Now, the rendering has been delayed until later, and
so it is no longer a responsibility of `psc-publish`.

This means that the information stored in the JSON output by `psc-publish` now
includes values of types such as `Language.PureScript.Type` and
`Language.PureScript.Kind` in place of where it previously would have had
`RenderedCode`, and this gives us some more flexibility with respect to what we
do with it.

The purpose of this change is to allow transformations on types and kinds
inside the Pursuit server, in order to output Hoogle files which can be
understood as Haskell; for the time being, this is the approach we're pursuing,
since the type systems of Haskell and PureScript are very similar. For example,
a type synonym such as the following:

    type Person a = { name :: String, age :: Int | a }

would currently be encoded like this (although this is very much a first pass):

    type Person a = Object (PS_Row (PS_Label_name String) (PS_Label_age Int) (PS_Row_Tail a))

Once we have things working, I will probably investigate whether other
encodings are more effective.

## The Pursuit server

Phil Freeman has now deployed a pre-alpha version of pursuit to
<http://new-pursuit.purescript.org>. Feel free to have a look around, but
please don't start trying to actually use it just yet. Additionally, trying to
actually deploy it has revealed various deficiencies; generally, deployment
seems harder than it ought to be. For example, setting configuration values
such as the port to listen on, or the data directory, or OAuth tokens for the
Github API is a bit painful. There's a weird mixture of parsing and using
configuration settings at both compile time and run time that I haven't fully
understood, and (as unfortunately seems to often be the case with compile-time
code) the errors are not very clear. I'm planning on addressing this soon.

[aeson-better-errors on Hackage]: https://hackage.haskell.org/package/aeson-better-errors
[tutorial/introduction blog post]: /blog/aeson-better-errors/
[bower-json on Hackage]: https://hackage.haskell.org/package/bower-json
