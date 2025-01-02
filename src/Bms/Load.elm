module Bms.Load exposing (fromRawData, separateByMeasure, separeteLn)

import Array exposing (Array)
import Bms.TimeObject as TimeObject
import Bms.Types as Bms exposing (Bms, ChartType(..), Note, NoteType(..), Object, RawBms)
import Dict
import List.Extra as List
import Maybe.Extra as Maybe
import String.Extra as String
import String.Extra2 as String


fromRawData : RawBms -> Bms
fromRawData { name, header, mlens, data } =
    let
        lines =
            List.maximumBy .measure data
                |> Maybe.map .measure
                |> Maybe.unwrap 1 (max (Dict.keys mlens |> List.last |> Maybe.map ((+) 1) |> Maybe.withDefault 0))
                |> List.range 0
                |> List.scanl (\x acc -> Maybe.unwrap TimeObject.resolution ((*) TimeObject.resolution) (Dict.get x mlens) + acc) 0
                |> Array.fromList

        ( notes, others ) =
            List.partition (\x -> 36 <= x.channel && x.channel < 36 * 3 || 5 * 36 <= x.channel && x.channel < 7 * 36) data

        notes_ =
            List.map
                (\x ->
                    let
                        value =
                            String.baseFromString 36 x.value
                    in
                    { time = TimeObject.fromMeasureAndFraction lines x.measure x.fraction
                    , measure = x.measure
                    , value = value
                    , ext =
                        toNoteType x.channel
                            value
                    }
                )
                notes

        toNoteType ch v =
            if 36 <= ch && ch < 3 * 36 then
                Normal <| ch - 36

            else if 5 * 36 <= ch && ch < 7 * 36 then
                Long (ch - 5 * 36) 0 v

            else
                Normal 0

        bpm =
            Dict.get "bpm" header
                |> Maybe.andThen List.last
                |> Maybe.andThen
                    String.toFloat
                |> Maybe.withDefault 130

        chartType =
            let
                maxKey =
                    Maybe.withDefault 0 <| List.maximum <| List.map (Bms.key << .ext) notes_

                extension =
                    String.rightOfBack "." name
            in
            if extension == "pms" then
                Key9
                -- とりあえず，5keyのフリーゾーンを無視する

            else if maxKey >= 36 + 7 || maxKey >= 36 && extension == "bme" then
                Key14

            else if maxKey >= 36 then
                Key10

            else if maxKey >= 7 || extension == "bme" then
                Key7

            else
                Key5

        lnobj =
            Dict.get "lnobj" header |> Maybe.andThen List.last |> Maybe.map (String.baseFromString 36)

        waves =
            Dict.filter (\k _ -> String.startsWith "wav" k) header
                |> Dict.toList
                |> List.filterMap (\( k, v ) -> Maybe.map (Tuple.pair <| String.baseFromString 36 (String.dropLeft 3 k)) <| List.last v)
                |> Dict.fromList
    in
    { name = name
    , chartType = chartType
    , header = header
    , bpm = bpm
    , lnobj = lnobj
    , waves = waves
    , lines = lines
    , notes = Maybe.unwrap identity ln2 lnobj <| ln1 <| List.map (adjustKey chartType) notes_
    , others = others
    }


adjustKey : ChartType -> Note -> Note
adjustKey ct note =
    let
        ext =
            Bms.setKey (Bms.adjustKey ct <| Bms.key note.ext) note.ext
    in
    { note | ext = ext }


separeteLn : Array Float -> List Note -> List Note
separeteLn lines =
    let
        f x notes =
            case x.ext of
                Long _ l tv ->
                    g l tv x notes

                _ ->
                    x :: notes

        g l tv note notes =
            let
                measureStart =
                    Maybe.withDefault 0 (Array.get note.measure lines)

                measureEnd =
                    Maybe.withDefault (measureStart + TimeObject.resolution) (Array.get (note.measure + 1) lines)
            in
            if l > measureEnd - note.time then
                { time = note.time
                , measure = note.measure
                , value = note.value
                , ext = Long (Bms.key note.ext) (measureEnd - note.time) tv
                }
                    :: g (l - (measureEnd - note.time)) tv { note | time = note.time + measureEnd - measureStart, measure = note.measure + 1 } notes

            else
                { time = note.time, measure = note.measure, value = note.value, ext = Long (Bms.key note.ext) l tv } :: notes
    in
    List.foldr f [] >> Bms.sort


separateByMeasure : List (Object x v) -> List ( Int, List (Object x v) )
separateByMeasure =
    List.groupWhile (\a b -> a.measure == b.measure) >> List.map (\( a, b ) -> ( a.measure, a :: b ))


ln1 : List Note -> List Note
ln1 =
    let
        f note ( state, notes ) =
            case note.ext of
                Long k l tv ->
                    if l == 0 then
                        case Dict.get k state of
                            Just ( v, tv_ ) ->
                                ( Dict.remove k state, { note | ext = Long k (TimeObject.diff v note) tv_ } :: notes )

                            Nothing ->
                                ( Dict.insert k ( { time = note.time, measure = note.measure }, tv ) state, notes )

                    else
                        ( state, note :: notes )

                _ ->
                    ( state, note :: notes )
    in
    List.foldr f ( Dict.empty, [] ) >> Tuple.second


ln2 : Int -> List Note -> List Note
ln2 lnobj =
    let
        f note ( state, notes ) =
            if note.value == lnobj then
                ( Dict.insert (Bms.key note.ext) { time = note.time, measure = note.measure } state, notes )

            else
                case Dict.get (Bms.key note.ext) state of
                    Just v ->
                        ( Dict.remove (Bms.key note.ext) state, { note | ext = Long (Bms.key note.ext) (TimeObject.diff v note) lnobj } :: notes )

                    Nothing ->
                        ( state, note :: notes )
    in
    List.foldr f ( Dict.empty, [] ) >> Tuple.second
