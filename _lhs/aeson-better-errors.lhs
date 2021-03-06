* [aeson-better-errors on Hackage][]

I've just released a library on Hackage for building parsers using Aeson, which
should hopefully be able to provide much better errors than you'd get with
Aeson in most cases. Rather than go into specifics about the motivation of my
design, I'll just jump right in to an example &mdash; then, hopefully, the
reason I wrote it will become clear.

> {-# LANGUAGE OverloadedStrings #-}
> import Control.Applicative
> import Data.Aeson
> import Data.Monoid
> import qualified Data.Text as T
> import qualified Data.Text.IO as T
>
> import Data.Aeson.BetterErrors
>   (parse, Parse, ParseError, ParseError',
>     key, keyMay, keyOrDefault,
>     nth, nthMay, nthOrDefault,
>     asString, asIntegral,
>     withString,
>     eachInArray, eachInObject,
>     displayError, displayError',
>     toAesonParser, toAesonParser')

We'll start simple:

> data Person = Person String Int deriving (Show)

Suppose we have some JSON, like this: `{"name": "Bob", "age": 25}`

Then, we can write a parser in good old Applicative style:

> asPerson :: Parse e Person
> asPerson = Person <$> key "name" asString <*> key "age" asIntegral

Now let's suppose the encoding changes to this: `["Bob", 25]`. That's fine too:

> asPerson' :: Parse e Person
> asPerson' = Person <$> nth 0 asString <*> nth 1 asIntegral

Let's test these out:

~~~
λ: parse asPerson "{\"name\": \"Bob\", \"age\": 25}"
Right (Person "Bob" 25)

λ: parse asPerson' "[\"Angela\", 43]"
Right (Person "Angela" 43)

λ: parse asPerson "{\"name\": \"Bob\"}"
Left (BadSchema [] (KeyMissing "age"))

λ: parse asPerson "{\"name\": \"Bob\", \"age\": 25.1}"
Left (BadSchema [ObjectKey "age"] (ExpectedIntegral 25.1))

λ: parse asPerson "[\"Bob\", 25]"
Left (BadSchema [] (WrongType TyObject (Array (fromList [String "Bob",Number 25.0]))))
~~~

As you can see, `aeson-better-errors` uses a sum type for errors. Hooray! Let's
see what happens when we display those errors:

> printErr :: Either ParseError' v -> IO ()
> printErr (Right _) = T.putStrLn "Not an error."
> printErr (Left err) = mapM_ T.putStrLn (displayError' err)

~~~
λ: printErr $ parse asPerson "{\"name\": \"Bob\"}"
The required key "age" is missing

λ: printErr $ parse asPerson "[\"Bob\", 25]"
Type mismatch:
Expected a value of type object
Got: ["Bob",25]

λ: printErr $ parse asPerson "{\"name\": \"Bob\", \"age\": 25.1}"
At the path: ["age"]
Expected an integral value, got 25.1
~~~

The `displayError` function comes with the library, but all of the relevant
data types are exported, together with all of their constructors, so you can
easily write your own.

The errors also include the path in nested values, which can be really handy.
For example:

> deeplyNested :: Parse e Int
> deeplyNested = key "a" (nth 3 (key "b" (key "c" asIntegral)))

~~~
λ: printErr $ parse deeplyNested "{\"a\":[null,null,null,{\"b\":{\"c\":null}}]}"
At the path: ["a"][3]["b"]["c"]
Type mismatch:
Expected a value of type number
Got: null
~~~

Parsing homogenous arrays is also easy:

> data Class = Class
>   { classTeacher  :: Person
>   , classStudents :: [Person]
>   }
>   deriving (Show)
> 
> asClass :: Parse e Class
> asClass =
>   Class <$> key "teacher" asPerson'
>         <*> key "students" (eachInArray asPerson')

~~~
λ: parse asClass "{\"teacher\": [\"Mrs Brown\", 49], \"students\": [[\"George\", 12], [\"Abigail\", 13], [\"Michael\", 14]]}"
Right (Class {classTeacher = Person "Mrs Brown" 49, classStudents = [Person "George" 12,Person "Abigail" 13,Person "Michael" 14]})
~~~

As is handling indices which might not be present:

> possiblyAnInt :: Parse e (Maybe Int)
> possiblyAnInt = nthMay 2 asIntegral

> intWithDefault :: Parse e Int
> intWithDefault = nthOrDefault 2 14 asIntegral

~~~
λ: parse possiblyAnInt "[1,2,3,4]"
Right (Just 3)

λ: parse possiblyAnInt "[]"
Right Nothing

λ: printErr $ parse possiblyAnInt "{}"
Type mismatch:
Expected a value of type array
Got: {}

λ: parse intWithDefault "[]"
Right 14

λ: parse intWithDefault "[0,0,0]"
Right 0

λ: printErr $ parse intWithDefault "{}"
Type mismatch:
Expected a value of type array
Got: {}
~~~

Note that we still require that the value is an array, even if we aren't able
to get the item at index 2 out. 

The `eachInObject` and `keyMay` functions are analogous to these, but for
objects. 

`aeson-better-errors` also lets you define your own validations and errors, and
use them in parsers. For example, suppose I decide that people's names must be
at most 20 characters. I'd probably want to put my data types in such a way
that it's impossible to construct invalid values (for now, let's pretend it's
in a separate module and the constructor is not exported):

> newtype Name = Name String deriving (Show)
>
> data CustomParseError
>   = NameTooLong Int
>   deriving (Show)
>
> parseName :: String -> Either CustomParseError Name
> parseName s =
>   let len = length s
>   in if len <= 20
>         then Right (Name s)
>         else Left (NameTooLong len)
>
> data Person2 = Person2 Name Int deriving (Show)

Then, it's convenient to be able to validate inside the parser, and we can do
so:

> asPerson2 :: Parse CustomParseError Person2
> asPerson2 = Person2 <$> key "name" (withString parseName)
>                     <*> key "age" asIntegral
> 

~~~
λ: parse asPerson2 "{\"name\":\"Charlotte\", \"age\":25}"
Right (Person2 (Name "Charlotte") 25)

λ: parse asPerson2 "{\"name\":\"Charlotte Ooooooooooooo\", \"age\":25}"
Left (BadSchema [ObjectKey "name"] (CustomError (NameTooLong 23)))
~~~

Note that the error uses the `CustomError` constructor. All of our parsers up
to now have been polymorphic in the error type; that is, `Parse e X` for some
concrete type `X`. This means that, as long as I haven't subverted the type
system (don't worry, I haven't) they should work for _any_ choice of type `e`.
However, for the above parser, the error type is always `CustomError`.

What does this imply? Well, our `printErr` function doesn't work:

~~~
λ: printErr $ parse asPerson2 "{\"name\":\"Charlotte Ooooooooooooo\", \"age\":25}"
<interactive>:
    Couldn't match type ‘CustomParseError’ with ‘Data.Void.Void’
    [...]
~~~

This is because `printErr` only type checks if the parser used never produces
custom validation errors. The argument type of `printErr` is `Either
ParseError' v`; the type `ParseError'` is the type of parse errors which did
not arise as a result of custom validation errors. In this case, we have an
`Either (ParseError CustomParseError) v`, because custom validation errors can
occur, and those custom validation errors are values of the `CustomParseError`
type.

So how can we write a `printErr` function now? Easy &mdash; we just need to
supply a function for displaying our custom errors, and then we can use
`displayError` (the non-primed version) instead:

> displayMyCustomError :: CustomParseError -> T.Text
> displayMyCustomError (NameTooLong x) =
>   "The name was " <> T.pack (show x) <> " characters long, " <>
>     "but the maximum is 20 characters."
>
> printErrCustom :: Either (ParseError CustomParseError) v -> IO ()
> printErrCustom (Right _) = T.putStrLn "Not an error."
> printErrCustom (Left err) = mapM_ T.putStrLn (displayError displayMyCustomError err)

~~~
λ: printErrCustom $ parse asPerson2 "{\"name\":\"Charlotte Ooooooooooooo\", \"age\":25}"
At the path: ["name"]
The name was 23 characters long, but the maximum is 20 characters.
~~~

Last but not least, it's easy to make any `Parse e a` value into a `FromJSON a`
instance:

> instance FromJSON Person2 where
>   parseJSON = toAesonParser displayMyCustomError asPerson2

~~~
λ: decode "{\"name\":\"Charlotte\", \"age\":25}" :: Maybe Person2
Just (Person2 (Name "Charlotte") 25)

λ: eitherDecode "{\"name\":\"Charlotte Aaaaaaaaaaaaaaaah\", \"age\":25}" :: Either String Person2
Left "At the path: [\"name\"]\nThe name was 27 characters long, but the maximum is 20 characters.\n"
~~~

Note that there's also `toAesonParser'` for parsers which do not produce custom
validation errors.

More information and detailed documentation is available on [hackage][]. Enjoy!

[aeson-better-errors on Hackage]: https://hackage.haskell.org/package/aeson-better-errors
[hackage]: https://hackage.haskell.org/package/aeson-better-errors
