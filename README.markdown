## Data.Aeson.QQ ##

JSON quasiquatation for Haskell.

This package expose the function `aesonQQ` that compile time converts json
code into a `Data.Aeson.Value`.  `aesonQQ` got the signature

    aesonQQ :: QuasiQuoter

and is used like

    myCode = [aesonQQ| {age: 23, name: "John", likes: ["linux", "Haskell"]} |]

where it is important that

* you got no space in `[aesonQQ|` and
* no additional code after `|]`.

The quasiquatation can also bind to variables like

    myCode = [aesonQQ| {age: <|age|>, name: <|name|>} |]
    where age = 23 :: Integer
          name = "John"

where the function  `toJSON` will be called on `age` and `name` at runtime.

You can also insert Haskell code:

    myCode = [aesonQQ| {age: <|succ age|>, name: <|map toUpper name|>} |]
    where age = 23 :: Integer
          name = "John"

If you want to replace the name of the key in a hash you'll use the $-syntax:

    foo = [aesonQQ| {$bar: 42} |]
    bar = "age"
