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
    { bandWidth = 0.3, kernelFunction = Kernel.gauss, inscreaseScratchOptions = Nothing }


defIncreaseScratchOptions : IncreaseScratchOptions
defIncreaseScratchOptions =
    { minDuration = 8 }


convert : ChartType -> Options -> Bms -> Bms
convert chartType options bms =
    let
        group =
            groupingNotes bms.header.waves bms.notes
                |> List.andThen (Clustering.rough bms.mlens)
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
                    minDuration bms.mlens notes
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
    List.sortBy (groupLength bms.mlens >> negate) willScratches |> f ( scratches, keys ++ willKeys ) |> (\( a, b ) -> a ++ b)


doujiOshi : ListNonempty Note -> Bool
doujiOshi =
    Nonempty.toList >> List.foldl2 (\x y acc -> acc && Maybe.unwrap True (BTime.eq x >> not) y) True


minDuration : Dict Int Float -> ListNonempty Note -> Float
minDuration mlens =
    Nonempty.toList >> List.foldl2 (\x y acc -> Maybe.unwrap acc (flip (BTime.diffWithMeasureLength mlens) x >> min acc) y) 1000


groupLength : Dict Int Float -> ListNonempty Note -> Float
groupLength mlens notes =
    let
        ( h, t ) =
            groupRange notes
    in
    BTime.diffWithMeasureLength mlens (Nonempty.last notes) (Nonempty.head notes)


isOverlappingGroup : ListNonempty Note -> ListNonempty Note -> Bool
isOverlappingGroup notes1 notes2 =
    if BTime.compare (Nonempty.head notes1) (Nonempty.last notes2) == GT || BTime.compare (Nonempty.head notes2) (Nonempty.last notes1) == GT then
        False

    else
        True


groupRange : ListNonempty Note -> ( Float, Float )
groupRange =
    let
        lastTimeOfNote note =
            case note.ext of
                Long _ l ->
                    BTime.toFloat note + l

                _ ->
                    BTime.toFloat note
    in
    Nonempty.foldl (\note -> Tuple.mapBoth (min (BTime.toFloat note)) (max (lastTimeOfNote note))) ( 0, 999999 )
