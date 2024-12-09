module Bms.Converter exposing
    ( convert
    , Options, IncreaseScratchOptions
    , defOptions, defIncreaseScratchOptions
    , groupingNotes
    )

{-| A converter of bms chart.

@docs convert
@docs Options, IncreaseScratchOptions
@docs defOptions, defIncreaseScratchOptions

-}

import BTime
import Basics.Extra exposing (..)
import Bms.Types exposing (Bms, ChartType, Note, key)
import Clustering
import Clustering.KernelFunction as Kernel
import Dict exposing (Dict)
import List.Extra as List
import List.Extra2 as List
import List.Nonempty as Nonempty exposing (ListNonempty)
import List.Nonempty.Extra as Nonempty
import Maybe.Extra as Maybe


type alias Options =
    { bandWidth : Float, kernelFunction : Float -> Float, inscreaseScratchOptions : Maybe IncreaseScratchOptions }


type alias IncreaseScratchOptions =
    { minDuration : Float }


defOptions : Options
defOptions =
    { bandWidth = 0.3, kernelFunction = Kernel.gauss, inscreaseScratchOptions = Nothing }


defIncreaseScratchOptions : IncreaseScratchOptions
defIncreaseScratchOptions =
    { minDuration = 0.125 }


convert : ChartType -> Options -> Bms -> Bms
convert chartType options bms =
    let
        group =
            groupingNotes bms.header.waves bms.notes
                |> List.andThen Clustering.rough
                |> List.andThen (Clustering.clustering options.bandWidth options.kernelFunction BTime.toFloat)
    in
    bms


groupingNotes : Dict Int String -> List Note -> List (ListNonempty Note)
groupingNotes wavs =
    let
        group =
            groupingByWaveFiles wavs
    in
    List.partition (.ext >> key >> (==) 0)
        >> (\( a, b ) -> Nonempty.gatherEqualsByList (group << .value) b |> (\tail -> Maybe.unwrap tail (flip (::) tail) <| Nonempty.fromList a))


groupingByWaveFiles : Dict Int String -> (Int -> Maybe Int)
groupingByWaveFiles wavs =
    let
        splited =
            List.map (\x -> ( x, String.split "_" x |> List.filter ((/=) "") )) <| List.unique <| Dict.values wavs

        f : List ( String, List String ) -> List (List ( String, List String ))
        f xs =
            let
                ys =
                    List.gatherBy (Tuple.second >> List.head) xs

                ( grouped, notGrouped ) =
                    List.partition (List.length >> (==) 1) ys
            in
            List.concat grouped
                :: List.concatMap (List.map (Tuple.mapSecond (List.tail >> Maybe.withDefault [])) >> f) notGrouped

        groupNumbers =
            f splited
                |> List.indexedMap (\i group -> List.map (\( a, _ ) -> Tuple.pair a i) group)
                |> List.concat
                |> Dict.fromList
    in
    flip Dict.get wavs >> Maybe.andThen (flip Dict.get groupNumbers)


doujiOshi : List Note -> Bool
doujiOshi =
    List.foldl2 (\x y acc -> acc && Maybe.unwrap True (BTime.eq x >> not) y) True


minDuration : List Note -> Float
minDuration =
    List.foldl2 (\x y acc -> Maybe.unwrap acc (flip BTime.diff x >> min acc) y) 1000


isOverlappingNote : List Note -> List Note -> Bool
isOverlappingNote notes1 notes2 =
    True
