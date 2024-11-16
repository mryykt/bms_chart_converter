module BTime exposing (..)

import Basics


type alias TimeObject x =
    { x | measure : Int, fraction : Float }


toFloat : TimeObject x -> Float
toFloat obj =
    Basics.toFloat obj.measure + obj.fraction


diff : TimeObject x -> TimeObject y -> Float
diff obj1 obj2 =
    toFloat obj1 - toFloat obj2


copyFrom : TimeObject x -> TimeObject y -> TimeObject y
copyFrom from to =
    { to | measure = from.measure, fraction = from.fraction }
