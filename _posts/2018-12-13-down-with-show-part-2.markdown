---
layout: post
title: "Down with Show! Part 2: What's wrong with the Show type class"
---

This is part two of three in a series where I will argue that it is time to
consign the `Show` type class to the dustbin of history. In [the previous
post][], I discussed a few rules of thumb which I use to help me decide whether
or not introducing a new type class is a good idea. In this post, I will
discuss the `Show` type class itself, and where I think it falls short.

We'll start by reminding ourselves what the class looks like (at least in
PureScript):

```haskell
class Show a where
  show :: a -> String
```

Haskellers may be wondering why I've missed out `showsPrec` and `showList`:
the answer is that they're not really relevant to this discussion.

### What is Show for?

I can think of at least three distinct situations where you might want a
function of the type `T -> String` for some type `T`:

* Serializing values: converting them to strings in order to pass them between
  programs
* Displaying values, for instance as part of a user interface
* Producing string representations of values for debugging purposes, or for use
  in the repl

While the `Show` class does, on occasion, get used for all of these three
things, I'm going to argue that it's not particularly good at any of them.

Firstly, it's important to recognise that these three purposes are actually
distinct; although a single function might suffice for all three purposes in
the cases of many basic types such as `Int` or `Bool`, this is not likely to
remain true once you start dealing with more complex types, and particularly
not for those types which are built out of sums and products (and you
inevitably *will* meet these sorts of types as soon as you start e.g. building
an application).

If you know Python, you might be aware of the difference between [`__str__`][]
and [`__repr__`][], which sort of goes along the same lines as the above. I
think Python was right to separate these functions: `__repr__` is designated as
being for debugging, whereas `__str__` is for 'nicely printable' string
representations of objects, which is perhaps closer to what I've described as
displaying values.

Let's look at each of these purposes in turn, and how effective the `Show`
class is in fulfilling them.

#### Serialization

Contrary to what [Real World Haskell: Chapter 6][] argues, I would say that
`Show` should not be used for serialization (and I don't expect this to be very
controversial). If you want to interface with any software at all not written
in Haskell, `Show` is going to be much more awkward than, say, JSON.  If you do
know you're only ever going to communicate between other Haskell programs using
the same data definitions, binary formats are likely to be a more efficient
alternative.

#### Display

To address this use case, I am going to refer back to the rules of thumb for
use of type classes from [the previous post][].

- Does it make sense as an abstraction? I'd argue that it doesn't, really;
  "display in a UI" is too vague and poorly specified to enable us to write
  many useful functions with a `Show` constraint. In most cases we will have to
  check that the `Show` instances of any types we are planning to use such a
  function with are appropriate, because in general they might not be.
- Does it have any laws? Not really, no. The `Show` type class has an informal
  law that where possible, `show x` should produce executable code which
  evaluates back to `x`, but most instances which satisfy this law will be
  useless for display as part of a user interface.
- Does it have at most one sensible, correct behaviour for a given type?
  Definitely not.  Even basic types like `Bool` have a multitude of options:
  `True` could be rendered as `"True"`, `"true"`, `"yes"`, `"1"`, and so on.
  `Double` also has a variety of options, depending on whether you want to use
  scientific notation, or how many decimal places are reasonable in the
  context. For dates or times, the format you want to use is often dependent
  on the user's locale as well as their individual settings (e.g. some users
  prefer times in the 24-hour format).
- Does it have any redundancy in its members' types? No; this is another reason
  I would argue that `show` has no place in production code. It's far too easy
  to accidentally `show` the wrong thing, and the type checker can't help you
  catch this.

I would argue, therefore, that displaying values as strings for use in e.g. a
user interface is a good example of a situation where regular functions are
more appropriate than a type class.

#### Debugging and the repl

So we've established that `Show` should not be used for serialization or
display, and we have seen better alternatives for both of these purposes.
That just leaves debugging and the repl to deal with.

Currently, in both Haskell and PureScript, the repl uses the `Show` type class
by default. Suppose I type something into the repl which evaluates to a type
`T`. If there is a `Show T` instance, then the result of `show`ing whatever my
expression evaluates to is printed. Otherwise, I get an error along the lines
of `No instance for (Show T)`.

From the perspective of showing values in the repl, the first major drawback
of `Show` is that lots of types don't have instances. This is perhaps a result
of it trying to do too much at once.

Any function being used for serialization must be *injective,* which means that
serializing two distinct values should give you two distinct results: this is
crucial if we want other programs to be able to accept what we produce and
reconstruct the same value.

If `Show` was originally intended to be used for serialization, then `show` of
course would have needed to be injective for every instance. However, this
rules out instances for a lot of types. For example, we can't write an
injective `Show` instance for functions `a -> b`, unless we know that `a` is
only inhabited by finitely many values (because we need to check what our
function does to each of these values). Even then, the output is very unlikely
to be useful unless the type `a` has a very small number of inhabitants. But
some of the most common types we use have a large number of inhabitants, or
even infinitely many! Consider e.g. `Int`, `Integer`, or `[a]`.

There are other examples, too. For example, there are a few types which we
can't do anything with unless we move into `IO`. The type `IO` itself is one:
we can't write an injective function `IO () -> String`, because we (rightly)
have no tools to introspect a value of type `IO ()`. Similarly, we can't write
an injective function `IORef Int -> String`. We can't even read the value of
the `IORef` at the time the function is evaluated and turn that into a string
for use in our return value, unless we cheat and use something like
`unsafePerformIO`, because we have to produce a `String`, not an `IO String`.

The fact that so many types don't have instances is a significant hindrance to
the purpose of seeing representations of values in the repl. You might argue
that showing functions in the repl would be useless anyway, because what would
we produce? However, it is very common to come across types which are products
or records where one field is a function, or an `IO T`, or any other type which
lacks a `Show` instance. In these cases, we can't derive a `Show` instance, so
if we want to be able to see values of this type in the repl, we would have to
manually write a `Show` instance which skips over the problematic fields, and
this is quite tiresome; in practice, we often don't bother. Another option is
to use orphan instances for problematic fields. For example, there is an orphan
`Show (a -> b)` instance in the Haskell module `Text.Show.Functions`, which
always produces the string `"<function>"`. This is fine in languages which
support orphan instances (note that PureScript does not), but far from ideal.
One reason to avoid orphan instances is that you can't opt-in to them on a
per-module basis; if I use an orphan instance in a certain module, then
everyone else who imports that module also has that instance in
scope, whether they want it or not.

Another problem with `Show` is that, for larger types, the output is very
difficult to extract useful information from &mdash; at least, it is if we're
using the derived instances. Here's an example of what happens if I try to
`show` a Pursuit package, for instance:

```
λ: latest <- handler getLatestPackages
λ: handler (uncurry lookupPackage (latest !! 1))
Right (Package {pkgMeta = PackageMeta {bowerName = PackageName "purescript-web-storage", bowerDescription = Nothing, bowerMain = [], bowerModuleType = [], bowerLicense = ["MIT"], bowerIgnore = ["**/.*","bower_components","node_modules","output","bower.json","package.json"], bowerKeywords = [], bowerAuthors = [], bowerHomepage = Just "https://github.com/purescript-web/purescript-web-storage", bowerRepository = Just (Repository {repositoryUrl = "git://github.com/purescript-web/purescript-web-storage.git", repositoryType = "git"}), bowerDependencies = [(PackageName "purescript-web-events",VersionRange {runVersionRange = "^1.0.0"}),(PackageName "purescript-nullable",VersionRange {runVersionRange = "^4.0.0"})], bowerDevDependencies = [], bowerResolutions = [], bowerPrivate = False}, pkgVersion = Version {versionBranch = [2,0,0], versionTags = []}, pkgVersionTag = "v2.0.0", pkgTagTime = Just 2018-05-25 14:05:13 UTC, pkgModules = [Module {modName = ModuleName [ProperName {runProperName = "Web"},ProperName {runProperName = "Storage"},ProperName {runProperName = "Event"},ProperName {runProperName = "StorageEvent"}], modComments = Nothing, modDeclarations = [Declaration {declTitle = "StorageEvent", declComments = Nothing, declSourceSpan = Just (SourceSpan {spanName = "/home/travis/build/purescript-web/purescript-web-storage/src/Web/Storage/Event/StorageEvent.purs", spanStart = SourcePos {sourcePosLine = 21, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 21, sourcePosColumn = 41}}), declChildren = [], declInfo = ExternDataDeclaration (NamedKind (Qualified (Just (ModuleName [ProperName {runProperName = "Prim"}])) (ProperName {runProperName = "Type"})))},Declaration {declTitle = "fromEvent", declComments = Nothing, declSourceSpan = Just (SourceSpan {spanName = "/home/travis/build/purescript-web/purescript-web-storage/src/Web/Storage/Event/StorageEvent.purs", spanStart = SourcePos {sourcePosLine = 23, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 23, sourcePosColumn = 41}}), declChildren = [], ...
```

The node.js repl handles cases like this one a lot better by using
pretty-printing, and by omitting details beyond a certain depth:

```
> require('tls')
{ CLIENT_RENEG_LIMIT: 3,
  CLIENT_RENEG_WINDOW: 600,
  SLAB_BUFFER_SIZE: 10485760,
  DEFAULT_CIPHERS: 'ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-AES256-GCM-SHA384:DHE-RSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-SHA256:DHE-RSA-AES128-SHA256:ECDHE-RSA-AES256-SHA384:DHE-RSA-AES256-SHA384:ECDHE-RSA-AES256-SHA256:DHE-RSA-AES256-SHA256:HIGH:!aNULL:!eNULL:!EXPORT:!DES:!RC4:!MD5:!PSK:!SRP:!CAMELLIA',
  DEFAULT_ECDH_CURVE: 'prime256v1',
  getCiphers: [Function],
  convertNPNProtocols: [Function],
  convertALPNProtocols: [Function],
  checkServerIdentity: [Function: checkServerIdentity],
  parseCertString: [Function: parseCertString],
  createSecureContext: [Function: createSecureContext],
  SecureContext: [Function: SecureContext],
  TLSSocket: { [Function: TLSSocket] super_: { [Function: Socket] super_: [Object] } },
  Server: { [Function: Server] super_: { [Function: Server] super_: [Object] } },
  createServer: [Function],
  connect: [Function],
  createSecurePair: [Function: deprecated] }
```

This is a slightly unfair example because the `tls` module is less complex than
the `Package` type in Pursuit, but hopefully my point is clear. The string
`[Object]` indicates that there is an object there, but it hasn't been printed
in full because it's too deep in the structure. If we want to see it, we can
drill down deeper into the structure:

```
> require('tls').Server
{ [Function: Server]
  super_: 
   { [Function: Server]
     super_: 
      { [Function: EventEmitter]
        EventEmitter: [Circular],
        usingDomains: true,
        defaultMaxListeners: [Getter/Setter],
        init: [Function],
        listenerCount: [Function] } } }
```

To me, this is an indication that a function of the type `T -> String` is the
wrong choice for debugging or for showing values in the repl. Ideally we would
be using a type which provides a tree structure, to allow us to do things like
cutting off pretty-printing once we reach a certain depth. Changing the class
to use a tree structure instead of just `String` also has the fortuitous effect
of making it less tempting to abuse the class for other purposes, such as
serialization or display.

I first encountered the idea of using a tree structure instead of a `String`
when [@rightfold][] suggested it in [a GitHub thread discussing this issue][].
This is a tremendously valuable insight; without it, I most likely never would
have managed to come up with what I'm going to show you in the next post.

### Summary

In this post, I've argued that the design of the `Show` class falls short on a
number of fronts: most importantly, that it tries to do too much at once, and
as a result, it's not particularly good at any of the things it tries to do.
I've also discussed its shortcomings from the perspective of showing things in
the repl or for debugging purposes, and I've hinted at how we might be able to
address these.

In the next post, I'll talk in more detail about a design I've come up with in
order to address these shortcomings.

Next up: [Part 3: A replacement for Show][]

[Real World Haskell: Chapter 6]: http://book.realworldhaskell.org/read/using-typeclasses.html
[Stack Overflow: Should I use typeclasses or not?]: https://stackoverflow.com/questions/17100036/should-i-use-typeclasses-or-not
[the previous post]: {{ site.baseurl }}{% post_url 2018-12-12-down-with-show-part-1 %}
[`__repr__`]: https://docs.python.org/3/reference/datamodel.html?highlight=__str__#object.__repr__
[`__str__`]: https://docs.python.org/3/reference/datamodel.html?highlight=__str__#object.__str__
[@rightfold]: https://github.com/rightfold
[a GitHub thread discussing this issue]: https://github.com/purescript/purescript/issues/1675#issuecomment-216758519
[Part 3: A replacement for Show]: {{ site.baseurl }}{% post_url 2018-12-13-down-with-show-part-3 %}
