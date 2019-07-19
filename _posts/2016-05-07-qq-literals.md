---
layout: post
title: User-defined literals in Haskell via QuasiQuotes
updated: 2017-12-22
---

A couple of days ago, somebody submitted a blog post to the Haskell subreddit,
titled [Abusing -XOverloadedStrings to Implement Type Directed Parsing](https://lambdasandcaches.blogspot.com/2016/05/abusing-xoverloadedstrings-to-implement.html).

I'm not totally sold on the 'type-directed' aspect (in fact, I'm generally
quite reluctant to use type classes for this sort of thing, but that's a topic
for another day). Also, unfortunately, it turns out that the mechanism
described in the above post can result in errors at runtime. However, I
certainly agree that some mechanism for embedding literal values for arbitrary
user-defined types ought to be a good thing, so I decided to investigate a
little more.

Someone else also pointed out the very impressive `refined` library:
<https://nikita-volkov.github.io/refined/>. However, I found it was just a
little awkward to use for this particular use case, which is perhaps not
surprising given that it appears to be designed for a slightly different one.

Having used Yesod's quasiquoters for HTML templates, and knowing that most
errors you can make in them get reported at compile time, it seemed to me like
`QuasiQuotes` would be an appropriate thing to build this on top of.

Here's what I came up with:

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module QQLiterals where

import Language.Haskell.TH (varE, Name)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

qqLiteral :: (String -> Either String a) -> Name -> QuasiQuoter
qqLiteral parse parseFn = QuasiQuoter {..}
  where
  quoteExp str =
    case parse str of
      Right _ -> [| case $(varE parseFn) str of { Right x -> x } |]
      Left err -> fail err

  quotePat  = unsupported "pattern"
  quoteType = unsupported "type"
  quoteDec  = unsupported "declaration"

  unsupported context = fail $
    "Unsupported operation: this QuasiQuoter can not be used in a " ++ context ++ " context."
```

To create a `QuasiQuoter`, you need to supply four functions: `quoteExp`,
`quotePat`, `quoteType`, and `quoteDec`. We only care about quoting expressions
here, so we use calls to `fail` in the others to ensure that they all fail
immediately if our quasiquoter is used in an unsupported context.

The `qqLiteral` function takes two arguments. The first is a function which
parses values of the type we're interested in, which should return a `Right`
value in case of success, or a `Left` value with an error message in case of
failure. The second is a `Name`, which must refer to the same function passed
as the first argument.

Whenever the resulting quasiquoter is used, our `quoteExp` function will be
applied, at compile time, to the string inside the quoter. The resulting `Q
Exp` value is then spliced back into the code.

So, our `quoteExp` function attempts to parse the string once at compile-time,
using the function argument. If that is successful, it chucks away the result,
since it's not possible in general to turn this value back into an expression
directly. Instead, it splices in an expression which should evaluate to the
same thing, by applying the parser to the same string (as a string literal
expression), and then performing an incomplete pattern match, matching only on
the `Right` constructor. This ought to be safe, because we already checked the
result is a `Right` value. If parsing is not successful, it calls `fail` which
causes a compile-time error.

Here's an example, using the `URI` type from `network-uri`:

```haskell
{-# LANGUAGE TemplateHaskell #-}
module Spec.Example where

import Network.URI (URI, parseURI)
import Language.Haskell.TH.Quote (QuasiQuoter)
import QQLiterals (qqLiteral)

eitherParseURI :: String -> Either String URI
eitherParseURI str =
  maybe (Left ("Failed to parse URI: " ++ str)) Right (parseURI str)

uri :: QuasiQuoter
uri = qqLiteral eitherParseURI 'eitherParseURI
```

Here, in order to provide the `Name` argument, we quote the `eitherParseURI`
function by prefixing it with a single-quote character.

Now, we can use the `uri` quasiquoter as follows:

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Spec.Example (uri)
import Network.URI (URI(..))

main :: IO ()
main = do
  let exampleDotCom = [uri|http://example.com/lol|]
  putStrLn ("scheme: " ++ uriScheme exampleDotCom)
  putStrLn ("authority: " ++ show (uriAuthority exampleDotCom))
  putStrLn ("path: " ++ uriPath exampleDotCom)
```

which produces the following output:

```
scheme: http:
authority: Just (URIAuth {uriUserInfo = "", uriRegName = "example.com", uriPort = ""})
path: /lol
```

Using it with an invalid URI will fail at compile time; replacing the
quasiquoted URI in the above program with `[uri|invalid|]` yields:

```
$ stack build
<snip>
[2 of 2] Compiling Main             ( test/Spec.hs, .stack-work/dist/x86_64-osx/Cabal-1.22.5.0/build/qq-literals-test/qq-literals-test-tmp/Main.o )
/Users/hgarrood/Documents/code/qq-literals/test/Spec.hs:8:28: Failed to parse URI: invalid
```

I'm quite pleased with how this experiment turned out. I especially like that
it (mostly) insulates users from the perils of Template Haskell. Of course,
as a consumer of this, you yourself have to ensure that the `Name` argument
does in fact refer to the same function, which is not ideal (and I would love
to hear if there's a better way of doing this). I might release this on Hackage
if people think it's a good enough idea. I've put the whole thing up on GitHub:
<https://github.com/hdgarrood/qq-literals/>.

Thanks to everyone who participated in the original thread, without which this
definitely wouldn't have occurred to me.

## Update

This is now on Hackage: <https://hackage.haskell.org/package/qq-literals/>.
