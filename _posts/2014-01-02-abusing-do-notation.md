---
title: (Ab)using do notation for a Wai DSL
layout: post
---

Recently I was thinking it would be nice to have something like Rack's [URLMap]
for Wai. If you haven't come across it, it lets you combine Rack applications
easily, based on the request path info or host. For example, if you wanted to
have a bug tracking application under "/bugs", and a helpdesk application under
"/helpdesk", and your main website under "/", you might have:

```ruby
Rack::URLMap.new do
  map "/bugs" do
    run BugTrackingApp
  end

  map "/helpdesk" do
    run HelpdeskApp
  end

  map "/" do
    run MainSiteApp
  end
end
```

This URLMap can then become a single Rack application.

The first question is how we are going to represent this data structure in
Haskell. Intuitively, it seems sensible that a request should start top of the
block, trying to match requests, and work its way downwards. A `Data.Map`
certainly won't do, since the order we get the keys out will probably not be
the same as the order they went in. Using the Ruby example above, this could
mean that a request meant for either of the applications on the sub-URIs might
end up being sent to the main site -- not good.

So we need an ordered lookup list, mapping request paths to Wai Applications.
This is what I used:

```haskell
type Path = [Text]
type UrlMap = [(Path, Application)]
```

Strict text makes things easier for us, since all we need to do to get the
request path as a list of strict Text values is call `pathInfo` on it. The
UrlMap type is also convenient because there is already a `Prelude` function
which can do the lookup for us: `lookup`; which takes a key and a lookup list,
and possibly returns the value associated with that key (that is, `Eq a => a ->
[(a,b)] -> Maybe b`).

If we want to use do notation, we need a monad to store this data. It should be
able to append information to a data structure which can then be extracted by
running the computation. Sounds like a job for the [Writer] monad, from
`Control.Monad.Writer`:

```haskell
type UrlMapM = Writer UrlMap ()
```

We don't care about the result of the computation, just the value that was
built up over the course of it, so we use unit `()` as the second type
argument.

We don't want the users of our URL mapper to have to know the implementation
details, so let's provide some functions to abstract them away.

```haskell
mount :: Path -> Application -> UrlMapM
mount prefix app = tell [(prefix, app)]

runUrlMapM :: UrlMapM -> UrlMap
runUrlMapM = execWriter
```

So now we can do this:

```haskell
urlMapM :: UrlMapM
urlMapM = do
    mount ["bugs"] bugTrackingApp
    mount ["helpdesk"] helpdeskApp
    mount [] mainSiteApp

urlmap :: UrlMap
urlmap = runUrlMapM urlMapM
```

Now to turn an `UrlMap` into an `Application`. When we're trying to match a
request with an application, we should work our way down the list, seeing if
the path an application is mounted under is a prefix of the request path; if
so, the prefix should be removed, and the request should be sent to that
application.

```haskell
try :: Eq a
    => [a]              -- ^ Path info of request
    -> [([a], b)]       -- ^ The UrlMap
    -> Maybe ([a], b)   -- ^ A pair consisting of the remainder of the path
                        -- after removing the matching prefix and the
                        -- relevant application, or Nothing.
try xs tuples = foldl go Nothing tuples
  where
    go (Just x) _     = Just x
    go _ (prefix, y)  = fmap (\xs' -> (xs', y)) $ stripPrefix prefix xs
```

`stripPrefix` from `Data.List` takes two lists, and, if the first is a prefix
of the second, removes the prefix from the second and returns it as a `Just`
value. If not, it returns Nothing.

Here I'm using the `Functor` instance for `Maybe`; if `stripPrefix` returns a
`Just` value, then `fmap` will apply the lambda function to the value inside
the `Just`. If it returns `Nothing`, then `fmap` will just return `Nothing`.

Now we just need to combine this function with a Wai `Request` and an `UrlMap`:

```haskell
toApplication :: UrlMap -> Application
toApplication urlmap = \req ->
    case try (pathInfo req) urlmap of
        Just (newPath, app) ->
            app $ req { pathInfo = newPath
                      , rawPathInfo = makeRaw newPath
                      }

        Nothing ->
            return $ responseLBS
                status500
                [("content-type", "text/plain")]
                ("WaiUrlMapper: no routes matched. Consider using " <>
                 "an empty path for the last mapping in the 'do' block.\n")

    where
    makeRaw :: [Text] -> B.ByteString
    makeRaw = ("/" `B.append`) . T.encodeUtf8 . T.intercalate "/"
```

One more nice helper function:

```haskell
mapUrls :: UrlMapM -> Application
mapUrls = toApplication . runUrlMapM
```

I'm not sure if it's absolutely necessary to modify both the `pathInfo` and the
`rawPathInfo`, but it seems safer to do so.

Here's the full code which I'm now using. It has a couple of additions: namely,
a `ToApplication` typeclass so that you can `mount` another `UrlMapM` under a
request path, and also a couple of extra helper functions which are just little
wrappers around `mount`.

```haskell
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module WaiUrlMapper where

import Control.Monad.Writer
import Data.Monoid
import Data.Char
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

type Path = [Text]
type UrlMap = [(Path, Application)]
type UrlMapM = Writer UrlMap ()

mount :: ToApplication a => Path -> a -> UrlMapM
mount prefix thing = tell [(prefix, toApplication thing)]

-- A little helper function, since most of the time, apps are mounted under
-- a single path segment.
mount' :: ToApplication a => Text -> a -> UrlMapM
mount' prefix thing = mount [prefix] thing

-- Another little helper function. Use this for the last mounted
-- application in the block, to avoid 500 errors from none matching.
mountRoot :: ToApplication a => a -> UrlMapM
mountRoot = mount []

runUrlMapM :: UrlMapM -> UrlMap
runUrlMapM = execWriter

try :: Eq a
    => [a]        -- ^ Path info of request
    -> [([a], b)] -- ^ List of applications to match
    -> Maybe ([a], b)
try xs tuples = foldl go Nothing tuples
    where
        go (Just x) _     = Just x
        go _ (prefix, y)  = stripPrefix prefix xs >>= \xs' -> return (xs', y)

class ToApplication a where
    toApplication :: a -> Application

instance ToApplication Application where
    toApplication = id

instance ToApplication UrlMap where
    toApplication urlMap = \req ->
        case try (pathInfo req) urlMap of
            Just (newPath, app) ->
                app $ req { pathInfo = newPath
                          , rawPathInfo = makeRaw newPath
                          }
            Nothing ->
                return $ responseLBS
                    status500
                    [("content-type", "text/plain")]
                    ("WaiUrlMapper: no routes matched. Consider using " <>
                     "'mountRoot for the last mapping in the 'do' block.\n")

        where
        makeRaw :: [Text] -> B.ByteString
        makeRaw = ("/" `B.append`) . T.encodeUtf8 . T.intercalate "/"

instance ToApplication UrlMapM where
    toApplication = toApplication . runUrlMapM

mapUrls :: UrlMapM -> Application
mapUrls = toApplication

-- And here's some example code which uses it:

trivialApp :: BL.ByteString -> Application
trivialApp msg req = return $
    responseLBS
        status200
        [("content-type", "text/plain")]
        (msg <>
            "\nrawPathInfo: " <> strictToLazy (rawPathInfo req) <>
            "\npathInfo: " <> stringToLBS (show $ pathInfo req) <>
            "\n")

    where
    strictToLazy :: B.ByteString -> BL.ByteString
    strictToLazy = BL.fromChunks . (: [])

    stringToLBS :: String -> BL.ByteString
    stringToLBS = BL.pack . map (fromIntegral . ord)

bugsApp, helpdeskApp, apiV1, apiV2, mainApp :: Application
bugsApp     = trivialApp "this is the bugs app"
helpdeskApp = trivialApp "this is the helpdesk app"
apiV1       = trivialApp "api, version 1"
apiV2       = trivialApp "api, version 2"
mainApp     = trivialApp "this is the main site"

urlmap :: UrlMapM
urlmap = do
    mount' "bugs" bugsApp
    mount' "helpdesk" helpdeskApp
    mount' "api" $ do
        -- Note that (by design) this cannot 'fall up' into the outer do
        -- block. So if we get here, it will have to either match the mapping
        -- below, or we'll get a 500 error.
        mount' "v1" apiV1
        mount' "v2" apiV2
    mountRoot mainApp

main :: IO ()
main = run 3000 $ mapUrls urlmap
```

[URLMap]: http://rack.rubyforge.org/doc/Rack/URLMap.html
[Writer]: http://learnyouahaskell.com/for-a-few-monads-more#writer 
