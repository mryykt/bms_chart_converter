module BTime exposing (..)

import Basics
import Dict exposing (Dict)
import Maybe


type alias TimeObject x =
    { x | measure : Int, fraction : Float }


toFloat : TimeObject x -> Float
toFloat obj =
    Basics.toFloat obj.measure + obj.fraction


fromFloat : Dict Int Float -> Float -> TimeObject {}
fromFloat mlens v =
    let
        measure =
            floor v
    in
    { measure = measure, fraction = Maybe.withDefault 1.0 (Dict.get measure mlens) * (v - Basics.toFloat measure) }


compare : TimeObject x -> TimeObject y -> Basics.Order
compare obj1 obj2 =
    Basics.compare (toFloat obj1) (toFloat obj2)


eq : TimeObject x -> TimeObject y -> Bool
eq obj1 obj2 =
    toFloat obj1 == toFloat obj2


diffWithMeasureLength : Dict Int Float -> TimeObject x -> TimeObject y -> Float
diffWithMeasureLength mlens obj1 obj2 =
    if obj1.measure == obj2.measure || obj1.fraction == 0 then
        (toFloat obj1 - toFloat obj2) * Maybe.withDefault 1.0 (Dict.get obj2.measure mlens)

    else
        diffWithMeasureLength mlens { obj1 | measure = obj2.measure + 1, fraction = 0 } obj2 + diffWithMeasureLength mlens obj1 { obj2 | measure = obj2.measure + 1, fraction = 0 }


copyFrom : TimeObject x -> TimeObject y -> TimeObject y
copyFrom from to =
    { to | measure = from.measure, fraction = from.fraction }
