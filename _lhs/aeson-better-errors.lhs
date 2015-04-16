* [aeson-better-errors on Hackage][]

I've just released a library on Hackage for building parsers using Aeson, which
should hopefully be able to provide much better errors in most cases. Rather
than go into specifics about the motivation of my design, I'll just jump right
in to an example &mdash; then, hopefully, the reason I wrote it will become
clear.

> import Control.Applicative
> import Data.Aeson
> import Data.Aeson.BetterErrors

We'll start simple:

> data Person = Person String Int deriving (Show)

Suppose we have the following JSON:

~~~
{"name": "Bob", "age": 25}
~~~

Then, we can write a parser in good old Applicative style:

> asPerson :: Parse () Person
> asPerson = Person <$> key "name" asString <*> key "age" asIntegral

However, what if 

- Often you want to do more than just parse JSON
  - Even after extracting the relevant bits from a JSON value, you may want to
    perform further validations before returning a value
  - Unfortunately your only choice here is to use String if an error occurs
  - this might lead to code like `if "expected integer" `isInfixOf` err` -
    yuck.
- Error messages Aeson gives are not very good
  - eg, it can say "got string, expected object", but it doesn't tell you where
    in the object

aeson-better-errors fixes both of these.

[aeson-better-errors on Hackage]: https://hackage.haskell.org/package/aeson-better-errors
