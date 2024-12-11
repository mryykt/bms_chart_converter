module Basics.Extra2 exposing (..)


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
