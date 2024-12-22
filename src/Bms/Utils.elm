module Bms.Utils exposing (..)

import Array exposing (Array)
import Basics.Extra2 exposing (..)
import Maybe.Extra as Maybe


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


baseToString : Int -> Int -> String
baseToString n x =
    let
        f d =
            if 0 <= d && d <= 9 then
                String.fromInt d

            else
                String.fromChar <| Char.fromCode <| Char.toCode 'a' + (d - 10)
    in
    if x >= n then
        baseToString n (x // n) ++ f (modBy n x)

    else
        f x


measureLength : Array Float -> Int -> Float
measureLength lines measure =
    Maybe.map2 (-) (Array.get (measure + 1) lines) (Array.get measure lines)
        |> Maybe.withDefault 960
