module Clustering exposing (..)

import BTime
import Bms.Types exposing (Note)
import List.Extra as List
import List.Extra2 as List


rough : List Note -> List (List Note)
rough notes =
    List.groupOn (\a b -> BTime.diff b a < 1) notes


clustering : Float -> (Float -> Float) -> (a -> Float) -> List a -> List (List a)
clustering bandWidth kernel f xs =
    let
        times =
            List.map f xs

        dens =
            density kernel bandWidth times

        mint =
            List.head times |> Maybe.withDefault 0

        maxt =
            List.last times |> Maybe.withDefault 0

        lmps =
            localMinimumPoints dens mint maxt 0.1

        helper t ( group, ungroup ) =
            List.span (f >> (>) t) ungroup |> (\( group_, ungroup_ ) -> ( group ++ [ group_ ], ungroup_ ))
    in
    List.foldl helper ( [], xs ) lmps |> (\( a, b ) -> a ++ [ b ])


density : (Float -> Float) -> Float -> List Float -> Float -> Float
density kernel bandwidth xs x =
    let
        n =
            toFloat <| List.length xs
    in
    1 / (n * bandwidth) * List.sum (List.map (\xi -> kernel <| (x - xi) / bandwidth) xs)


localMinimumPoints : (Float -> Float) -> Float -> Float -> Float -> List Float
localMinimumPoints f left right step =
    let
        helper x gradient =
            if x > right then
                []

            else
                let
                    newX =
                        x + step

                    newGradient =
                        f newX - f x
                in
                helper newX newGradient
                    |> (if gradient <= 0.0 && newGradient >= 0.0 then
                            (::) newX

                        else
                            identity
                       )
    in
    Debug.log "" <| helper left 1


rangef : Float -> Float -> Float -> List Float
rangef minv maxv step =
    if minv < maxv then
        minv :: rangef (minv + step) maxv step

    else
        []
