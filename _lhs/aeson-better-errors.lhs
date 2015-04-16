* [aeson-better-errors on Hackage][]

I've just released a library on Hackage for building parsers using Aeson, which
should hopefully be able to provide much better errors in most cases. Rather
than go into specifics about the motivation of my design, I'll just jump right
in to an example &mdash; then, hopefully, the reason I wrote it will become
clear.

> {-# LANGUAGE OverloadedStrings #-}
> import Control.Applicative
> import Data.Aeson
> import Data.Aeson.BetterErrors
> import qualified Data.Text.IO as T

We'll start simple:

> data Person = Person String Int deriving (Show)

Suppose we have some JSON, like this: `{"name": "Bob", "age": 25}`

Then, we can write a parser in good old Applicative style:

> asPerson1 :: Parse () Person
> asPerson1 = Person <$> key "name" asString <*> key "age" asIntegral

Then, let's suppose the encoding changes to this: ["Bob", 25]. That's fine too:

> asPerson2 :: Parse () Person
> asPerson2 = Person <$> nth 0 asString <*> nth 1 asIntegral

Let's test these out:

~~~
λ: parse asPerson1 "{\"name\": \"Bob\", \"age\": 25}"
Right (Person "Bob" 25)

λ: parse asPerson2 "[\"Angela\", 43]"
Right (Person "Angela" 43)

λ: parse asPerson1 "{\"name\": \"Bob\"}"
Left (BadSchema [] (KeyMissing "age"))

λ: parse asPerson1 "{\"name\": \"Bob\", \"age\": 25.1}"
Left (BadSchema [ObjectKey "age"] (ExpectedIntegral 25.1))

λ: parse asPerson1 "[\"Bob\", 25]"
Left (BadSchema [] (WrongType TyObject (Array (fromList [String "Bob",Number 25.0]))))
~~~

As you can see, `aeson-better-errors` uses a sum type for errors. Hooray! Let's
see what happens when we display those errors:

> printErr (Right _) = T.putStrLn "Not an error."
> printErr (Left err) = mapM_ T.putStrLn (displayError (const "n/a") err)

~~~
λ: printErr $ parse asPerson1 "{\"name\": \"Bob\"}"
The required key "age" is missing.

λ: printErr $ parse asPerson1 "[\"Bob\", 25]"
Type mismatch:
Expected a value of type object
Got: ["Bob",25]

λ: printErr $ parse asPerson1 "{\"name\": \"Bob\", \"age\": 25.1}"
At the path: ["age"]
Expected an integral value, got 25.1
~~~

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
