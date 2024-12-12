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

import Basics.Extra exposing (..)
import Bms.TimeObject as TimeObject
import Bms.Types as Bms exposing (Bms, ChartType, Note, NoteType(..), key)
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
    { minDuration : Int }


defOptions : Options
defOptions =
    { bandWidth = TimeObject.resolution / 3, kernelFunction = Kernel.gauss, inscreaseScratchOptions = Nothing }


defIncreaseScratchOptions : IncreaseScratchOptions
defIncreaseScratchOptions =
    { minDuration = 8 }


convert : ChartType -> Options -> Bms -> Bms
convert chartType options bms =
    let
        group =
            groupingNotes bms.header.waves bms.notes
                |> List.andThen Clustering.rough
                |> List.andThen (Clustering.clustering options.bandWidth options.kernelFunction .time)
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


inscreaseScratch : IncreaseScratchOptions -> Bms -> List (ListNonempty Note) -> List (ListNonempty Note)
inscreaseScratch options bms groups =
    let
        ( scratches, keys ) =
            List.partition (Nonempty.all (.ext >> Bms.key >> (==) 0)) groups

        duration =
            1 / toFloat options.minDuration

        ( willScratches, willKeys ) =
            List.partition
                (\notes ->
                    minDuration notes
                        <= duration
                        && List.all (not << isOverlappingGroup notes) scratches
                )
                keys

        f ( scratches_, keys_ ) willScratches_ =
            case willScratches_ of
                x :: xs ->
                    f ( x :: scratches_, keys_ ) (List.filterNot (isOverlappingGroup x) xs)

                [] ->
                    ( scratches_, keys_ )
    in
    List.sortBy (groupLength >> negate) willScratches |> f ( scratches, keys ++ willKeys ) |> (\( a, b ) -> a ++ b)


doujiOshi : ListNonempty Note -> Bool
doujiOshi =
    Nonempty.toList >> List.foldl2 (\x y acc -> acc && Maybe.unwrap True (.time >> (/=) x.time) y) True


minDuration : ListNonempty Note -> Float
minDuration =
    Nonempty.toList >> List.foldl2 (\x y acc -> Maybe.unwrap acc (flip TimeObject.diff x >> min acc) y) 1000


groupLength : ListNonempty Note -> Float
groupLength notes =
    groupRange notes |> uncurry (flip (-))


isOverlappingGroup : ListNonempty Note -> ListNonempty Note -> Bool
isOverlappingGroup notes1 notes2 =
    let
        ( h1, t1 ) =
            groupRange notes1

        ( h2, t2 ) =
            groupRange notes2
    in
    if h1 > t2 || h2 > t1 then
        False

    else
        True


groupRange : ListNonempty Note -> ( Float, Float )
groupRange =
    let
        lastTimeOfNote note =
            case note.ext of
                Long _ l ->
                    note.time + l

                _ ->
                    note.time
    in
    Nonempty.foldl (\note -> Tuple.mapBoth (min note.time) (max (lastTimeOfNote note))) ( 0, 999999 )
