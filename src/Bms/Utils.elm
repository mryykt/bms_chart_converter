module Bms.Utils exposing (..)

import Array exposing (Array)
import Basics.Extra2 exposing (..)
import List.Nonempty as Nonempty
import List.Nonempty.Extra as Nonempty
import Maybe.Extra as Maybe
import String.Extra as String


base : Int -> String -> Int
base n =
    let
        f c a =
            let
                code =
                    Char.toCode c
            in
            n
                * a
                + (if Char.toCode '0' <= code && code <= Char.toCode '9' then
                    code - Char.toCode '0'

                   else if Char.toCode 'a' <= code && code <= Char.toCode 'z' then
                    code - Char.toCode 'a' + 10

                   else
                    code - Char.toCode 'A' + 10
                  )
    in
    String.foldl f 0


{-| 2桁のものしか考えない
-}
baseToString : Int -> Int -> String
baseToString n x =
    let
        f d =
            if 0 <= d && d <= 9 then
                String.fromInt d

            else
                String.fromChar <| Char.fromCode <| Char.toCode 'a' + (d - 10)
    in
    f (x // n) ++ f (modBy n x)


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


measureLength : Array Float -> Int -> Float
measureLength lines measure =
    Maybe.map2 (-) (Array.get (measure + 1) lines) (Array.get measure lines)
        |> Maybe.withDefault 960
