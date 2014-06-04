# aeson-qq: JSON quasiquoter for Haskell

This package expose the function
[`aesonQQ`](http://hackage.haskell.org/package/aeson-qq/docs/Data-Aeson-QQ.html#v:aesonQQ)
that compile-time converts a string representation of a JSON value into
[`Data.Aeson.Value`](http://hackage.haskell.org/package/aeson-0.7.0.6/docs/Data-Aeson.html#t:Value).
`aesonQQ` got the signature

```haskell
aesonQQ :: QuasiQuoter
```

and is used like

~~~ {.haskell}
{-# LANGUAGE QuasiQuotes #-}
import Data.Aeson.QQ
import Data.Aeson (Value)

john :: Value
john = [aesonQQ| {age: 23, name: "John", likes: ["linux", "Haskell"]} |]
~~~

The quasiquoter can also interpolate variables like

~~~ {.haskell}
jane :: Value
jane = [aesonQQ| {age: #{age}, name: #{name}} |]
  where
    age = 23 :: Int
    name = "Jane"
~~~

where the function
[`toJSON`](http://hackage.haskell.org/package/aeson-0.7.0.6/docs/Data-Aeson.html#v:toJSON).
will be called on `age` and `name` at runtime.

You can also interpolate arbitrary Haskell expressions:

~~~ {.haskell}
mary :: Value
mary = [aesonQQ| {age: #{succ age}, name: "Mary"} |]
  where
    age = 23 :: Int
~~~

If you want to replace the name of the key in a hash you'll use the $-syntax:

~~~ {.haskell}
joe :: Value
joe = [aesonQQ| {$key: 23, name: "Joe"} |]
  where
    key = "age"
~~~
