module Bms.Converter.Clustering exposing (..)

import Basics.Extra2 exposing (lessThan)
import Bms.Converter.Clustering.KernelFunction as Kernel exposing (KernelFunction)
import Bms.TimeObject as TimeObject
import List.Extra as List
import List.Extra2 as List
import List.Nonempty as Nonempty exposing (ListNonempty)
import List.Nonempty.Extra as Nonempty


clustering : Float -> KernelFunction -> (a -> Float) -> ListNonempty a -> List (ListNonempty a)
clustering bandWidth kernel f xs =
    let
        times =
            Nonempty.map f xs

        dens =
            density (Kernel.fromVariant kernel) bandWidth <| Nonempty.toList times

        mint =
            Nonempty.head times

        maxt =
            Nonempty.last times

        lmps =
            localMinimumPoints dens mint maxt (TimeObject.resolution / 10)

        helper t ( group, ungroup ) =
            List.span (f >> lessThan t) ungroup |> (\( group_, ungroup_ ) -> ( group ++ [ group_ ], ungroup_ ))
    in
    List.foldl helper ( [], Nonempty.toList xs ) lmps |> (\( a, b ) -> a ++ [ b ]) |> List.filterMap Nonempty.fromList


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
    helper left 1


rangef : Float -> Float -> Float -> List Float
rangef minv maxv step =
    if minv < maxv then
        minv :: rangef (minv + step) maxv step

    else
        []
