module Bms.Converter exposing (convert, groupingNotes)

import Basics.Extra exposing (..)
import Bms.Types exposing (Bms, ChartType, Note, key)
import Dict exposing (Dict)
import List.Extra as List
import List.Extra2 as List


type alias Option =
    {}


convert : ChartType -> Option -> Bms -> Bms
convert =
    Debug.todo ""


groupingNotes : Dict Int String -> List Note -> List (List Note)
groupingNotes wavs =
    let
        group =
            groupingByWaveFiles wavs
    in
    List.partition (.ext >> key >> (==) 0) >> (\( a, b ) -> a :: List.gatherBy (group << .value) b)


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
