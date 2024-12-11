module Bms.TimeObject exposing (..)

import Array exposing (Array)
import Basics.Extra2 exposing (..)
import Bms.Utils as Bms
import Maybe.Extra as Maybe


type alias TimeObject x =
    { x | time : Float, measure : Int }


resolution : number
resolution =
    960


diff : TimeObject x -> TimeObject y -> Float
diff obj1 obj2 =
    obj1.time - obj2.time


fromMeasureAndFraction : Array Float -> Int -> Float -> Float
fromMeasureAndFraction lines measure fraction =
    let
        measureStart =
            Maybe.withDefault 0 (Array.get measure lines)

        measureEnd =
            Maybe.withDefault (measureStart + resolution) (Array.get (measure + 1) lines)
    in
    measureStart + fraction * (measureEnd - measureStart)


getFraction : Array Float -> TimeObject x -> Float
getFraction lines obj =
    let
        measureStart =
            Maybe.withDefault 0 (Array.get obj.measure lines)

        measureEnd =
            Maybe.withDefault (measureStart + resolution) (Array.get (obj.measure + 1) lines)
    in
    (obj.time - measureStart) / (measureEnd - measureStart)
