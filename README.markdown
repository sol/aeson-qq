## Data.Aeson.QQ ##

JSON quasiquatation for Haskell.

This package expose the function `aesonQQ` that compile time converts json code into a `Data.Aeson.Value`.
`aesonQQ` got the signature

    aesonQQ :: QuasiQuoter

and is used like

    myCode = [aesonQQ| {age: 23, name: "Pelle", likes: ["mac","Haskell"] } |]

where it is important that

* you got no space in `[aesonQQ|` and
* no additional code after `|]`.

The quasiquatation can also bind to variables like

    myCode = [aesonQQ | {age: <| age |>, name: <| name |>} |]
     where age = 34 :: Integer
           name = "Pelle"

or contain Haskell code like

    myCode = [aesonQQ | {age: <| age + 42 :: Integer |>, name: <| map toUpper name |>} |]
     where age = 34 :: Integer
           name = "Pelle"
