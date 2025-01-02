module Bms.TimeObject exposing (..)

import Array exposing (Array)
import Basics.Extra2 exposing (..)
import Bms.Utils as Bms
import List.Nonempty as Nonempty
import List.Nonempty.Extra as Nonempty
import Maybe.Extra as Maybe


type alias TimeObject x =
    { x | time : Float, measure : Int }


resolution : number
resolution =
    960


diff : TimeObject x -> TimeObject y -> Float
diff obj1 obj2 =
    obj1.time - obj2.time


add : Array Float -> TimeObject x -> Float -> TimeObject x
add lines obj f =
    let
        measureStart =
            Maybe.withDefault 0 (Array.get obj.measure lines)

        measureEnd =
            Maybe.withDefault (measureStart + resolution) (Array.get (obj.measure + 1) lines)
    in
    if obj.time + f >= measureEnd then
        add lines { obj | measure = obj.measure + 1, time = measureEnd } (obj.time + f - measureEnd)

    else
        { obj | time = obj.time + f }


fromMeasureAndFraction : Array Float -> Int -> Float -> Float
fromMeasureAndFraction lines measure fraction =
    let
        measureStart =
            Maybe.withDefault 0 (Array.get measure lines)

        measureEnd =
            Maybe.withDefault (measureStart + resolution) (Array.get (measure + 1) lines)
    in
    measureStart + fraction * (measureEnd - measureStart)


measureLength : Array Float -> Int -> Float
measureLength lines measure =
    Maybe.map2 (-) (Array.get (measure + 1) lines) (Array.get measure lines)
        |> Maybe.withDefault 960


getFraction : Array Float -> TimeObject x -> Float
getFraction lines obj =
    let
        measureStart =
            Maybe.withDefault 0 (Array.get obj.measure lines)

        measureEnd =
            Maybe.withDefault (measureStart + resolution) (Array.get (obj.measure + 1) lines)
    in
    (obj.time - measureStart) / (measureEnd - measureStart)


toRatio : Float -> { denominator : Int, numerator : Int }
toRatio x =
    let
        helper r =
            let
                x_ =
                    toFloat r.numerator / toFloat r.denominator
            in
            if x_ == x || r.denominator == 192 then
                r

            else
                helper <|
                    Nonempty.minimumBy (\v -> abs (toFloat v.numerator / toFloat v.denominator - x)) <|
                        Nonempty.fromPair { denominator = r.denominator + 1, numerator = r.numerator - 1 }
                            [ { denominator = r.denominator + 1, numerator = r.numerator }
                            , { denominator = r.denominator + 1, numerator = r.numerator + 1 }
                            ]
    in
    helper { denominator = 1, numerator = 0 }
