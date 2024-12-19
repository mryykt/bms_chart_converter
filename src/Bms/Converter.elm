module Bms.Converter exposing
    ( convert
    , groupingNotes
    )

import Basics.Extra exposing (..)
import Bms.Converter.Clustering
import Bms.Converter.Options exposing (IncreaseScratchOptions, Options)
import Bms.TimeObject as TimeObject
import Bms.Types as Bms exposing (Bms, ChartType, Note, NoteType(..), key)
import Dict exposing (Dict)
import List.Extra as List
import List.Extra2 as List
import List.Nonempty as Nonempty exposing (ListNonempty)
import List.Nonempty.Extra as Nonempty
import Maybe.Extra as Maybe


convert : ChartType -> Options -> Bms -> Bms
convert _ options bms =
    let
        groups =
            groupingNotes bms.header.waves bms.notes
                |> List.andThen (Bms.Converter.Clustering.clustering options.bandWidth options.kernelFunction .time)

        newGroups =
            groups
                |> (if options.inscreaseScratchOptions.enabled then
                        inscreaseScratch options.inscreaseScratchOptions.value

                    else
                        identity
                   )
    in
    { bms | notes = List.map Nonempty.toList newGroups |> List.concat |> Bms.sort }


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


inscreaseScratch : IncreaseScratchOptions -> List (ListNonempty Note) -> List (ListNonempty Note)
inscreaseScratch options groups =
    let
        ( scratchGroups, keyGroups ) =
            List.partition (Nonempty.all (.ext >> Bms.key >> (==) 0)) groups

        ( willScratchGroups, willKeyGroups ) =
            List.partition
                (\notes ->
                    minDurationOfGroup notes
                        >= TimeObject.resolution
                        / toFloat options.minDuration
                        && not (doujiOshi notes)
                        && List.all (not << isOverlappingGroup notes) scratchGroups
                )
                keyGroups

        f ( scratches_, keys_ ) willScratches =
            case willScratches of
                x :: xs ->
                    let
                        ( ks, ss ) =
                            List.partition (isOverlappingGroup x) xs
                    in
                    f ( Nonempty.map (\note -> { note | ext = Bms.setKey 0 note.ext }) x :: scratches_, ks ++ keys_ )
                        ss

                [] ->
                    ( scratches_, keys_ )
    in
    List.sortBy (groupLength >> negate) willScratchGroups
        |> f ( scratchGroups, willKeyGroups )
        |> (\( a, b ) -> a ++ b)


doujiOshi : ListNonempty Note -> Bool
doujiOshi =
    Nonempty.toList >> List.foldl2 (\x y acc -> acc || Maybe.unwrap False (.time >> (==) x.time) y) False


minDurationOfGroup : ListNonempty Note -> Float
minDurationOfGroup =
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
    not (h1 > t2 || h2 > t1)


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
    Nonempty.foldl (\note -> Tuple.mapBoth (min note.time) (max (lastTimeOfNote note))) ( 999999, 0 )
