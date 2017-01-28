module Infix exposing (..)

(<!>) : List a -> (a -> b) -> List b
(<!>) = flip List.map


(=>) : a -> b -> (a, b)
(=>) = (,)


(?:) : Maybe a -> a -> a
(?:) = flip Maybe.withDefault
