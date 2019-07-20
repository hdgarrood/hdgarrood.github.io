---
layout: post
title: Serving Fay with Scotty
---

My current project needed something like [snaplet-fay]; unfortunately, I
couldn't see a good way to use it with scotty. However I did browse through the
source and it seems pretty simple to implement, so I thought I'd write a scotty
version.

Here's the result: [scotty-fay].

And a basic example:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid
import qualified Data.Text.Lazy as T
import Web.Scotty
import Web.Scotty.Fay

main :: IO ()
main = scotty 3000 $ do
    serveFay $
        -- If the first segment of the request path matches this, try to serve
        -- Fay. Otherwise try the next route.
        under "/scotty-fay" .
        -- Specify the directory where your Fay files are.
        from "src/fay"

    get "/" $ do
        html $
            "<!doctype html>" <>
            "<html>" <>
            "<head>" <>
            "<script type=text/javascript src=/scotty-fay/HelloWorld.hs></script>" <>
            "</head>" <>
            "<body><h1>lol</h1></body>" <>
            "</html>"
```

## Is it any good?

It's... ok.

* It doesn't have any caching, so it has to compile your Fay modules from
  scratch on each request, which can take 2 seconds (or more).
* It doesn't know about package confs; you have to put fay-base into your
  global package database for it to work (although I do now know how to fix
  this)
* Compilation errors are awkward to identify and fix.

Having said that, it has been a nice asset.

## Things I learned

1. The argument to `serveFay` acts as the entire configuration; the method I
   decided on was:

   * export a number of functions of type `Config -> Config`
   * allow the user to compose them with normal function composition `(.)`
   * Take one function of type `Config -> Config` and apply it to the default
     value for `Config` to get the final configuration.

   This seems to work quite well. I quite like this approach so far (although I
   certainly wouldn't say it's been tested extensively).

2. The `Config` type is supposed to act as an abstraction for Fay's
   `CompileConfig`. `CompileConfig` has so many options, and most of them will
   always be set to a certain value, so I might as well not expose them, right?

   It turns out that this argument is invalid, especially when you make it
   before learning what the options all do. This system sucks; while working on
   multicopter, I've had to switch to scotty-fay, make a one-line change, rerun
   tests, build a source distribution, and install it into my multicopter
   sandbox. All that as opposed to making that same one-line change in
   multicopter itself.

3. "package confs" and "cabal sandboxes" are basically the same thing.
   scotty-fay needs to know about these because Fay needs to load fay-base, and
   if you don't tell it which package conf to use, it will use the global one
   (which will fail if you are using a sandbox and don't want to install
   fay-base globally).

4. I need to think about a better way to handle Fay compilation errors. Simply
   returning a 500 Internal Server Error with human-readable HTML in the
   response is a bit awkward for fixing errors during development. Options I'm
   currently aware of are:

   * logging to STDOUT
   * Copy snaplet-fay and wrap the whole thing in a `console.error()`.

   I need to do some more experimentation.

## What I'm going to do now

### Wai middleware

I've realised that building this on scotty isn't the best option. It adds
unnecessary dependencies (it uses a tiny amount of the scotty library), and
it's also unfriendly for Wai users who are using a different framework (eg
Yesod).

The solution is simple: Convert it to a Wai middleware.

### How?

The most obvious approach might look something like this:

```haskell
fay :: Middleware
fay app req =
    maybe (app req)
          (\f -> compile f >> serve f)
          (getFaySourcePath req)
    where
    -- If the request asks for some compiled Fay code, give Just <its path
    -- on the disk>; otherwise, Nothing.
    getFaySourcePath :: Request -> Maybe FilePath

    -- compile the Fay source referenced by the request and dump it to
    -- disk. If the file hasn't changed since the last time it was compiled,
    -- do nothing.
    compile :: FilePath -> IO ()

    -- Serve the static file which is the result of compiling the Fay module at
    -- the given FilePath.
    serve :: FilePath -> IO Response
```

Okay, seems easy enough. But there's a pattern here!

## Arbitrary preprocessing

I used [Sprockets] when I was building the Ruby version of multicopter. I'm
starting to miss it, especially with CSS (vs Sass), so I [asked about it on
Stack Overflow].

The recommended solution: build a Wai middleware, which, when asked for a
static file, will compile it if necessary and serve it. That sounds familiar!

Looking back at the code above, I need something which can:

* Make `getFaySourcePath` configurable, so that users can tell it how to get
  the source path for any of the filetypes we're trying to compile, and also
  what type of file it is
* Allow users to supply their own `compile :: FilePath -> IO CompileResult`
  actions for each file type, where `CompileResult` is probably something like
  `Success | Failure Text`
* Serve the file or report failures depending on the returned `CompileResult`

That is, I need to build Sprockets for Haskell/Wai.

After that, creating a middleware which can compile and serve normal static
files as well as preprocessing Fay and Sass (and anything else I feel like)
should be pretty straightforward.

[snaplet-fay]: https://github.com/faylang/snaplet-fay
[scotty-fay]: https://githu.com/hdgarrood/scotty-fay
[Sprockets]: https://github.com/sstephenson/sprockets/
[asked about it on Stack Overflow]: http://stackoverflow.com/questions/20772380/is-there-anything-like-sprockets-for-haskell/
