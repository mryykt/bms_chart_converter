module Basics.Extra2 exposing (..)

import Maybe.Extra


lessThan : comparable -> comparable -> Bool
lessThan =
    (>)


greaterThan : comparable -> comparable -> Bool
greaterThan =
    (<)


leThan : comparable -> comparable -> Bool
leThan =
    (>=)


geThan : comparable -> comparable -> Bool
geThan =
    (<=)


ifelse : Bool -> a -> a -> a
ifelse cond a b =
    if cond then
        a

    else
        b


just : Maybe a -> (a -> b) -> b -> b
just x f def =
    Maybe.Extra.unwrap def f x
