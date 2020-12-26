module Main

implicit someString : String
someString = "lol"

implicit natString : Integer -> String
natString _ = "**"

foo: {a: Type} -> {x: a} -> a
foo {x} = x 
