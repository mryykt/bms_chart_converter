module Bms.Load exposing (fromRawData, separateByMeasure, separeteLn)

import Array exposing (Array)
import Bms.TimeObject as TimeObject
import Bms.Types as Bms exposing (Bms, ChartType(..), Note, NoteType(..), Object, RawBms)
import Bms.Utils exposing (base)
import Dict
import List.Extra as List
import Maybe.Extra as Maybe
import String.Extra as String


fromRawData : RawBms -> Bms
fromRawData { name, headers, mlens, data } =
    let
        lines =
            List.maximumBy .measure data
                |> Maybe.unwrap 1 .measure
                |> List.range 0
                |> List.scanl (\x acc -> Maybe.unwrap TimeObject.resolution ((*) TimeObject.resolution) (Dict.get x mlens) + acc) 0
                |> Array.fromList
                |> Debug.log ""

        ( notes, others ) =
            List.partition (\x -> 36 <= x.channel && x.channel < 36 * 3 || 5 * 36 <= x.channel && x.channel < 7 * 36) data

        notes_ =
            List.map (\x -> { time = TimeObject.fromMeasureAndFraction lines x.measure x.fraction, measure = x.measure, value = base 36 x.value, ext = toNoteType x.channel }) notes

        toNoteType ch =
            if 36 <= ch && ch < 3 * 36 then
                Normal <| ch - 36

            else if 5 * 36 <= ch && ch < 7 * 36 then
                Long (ch - 5 * 36) 0

            else
                Normal 0

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
    in
    { chartType = chartType
    , header = headers
    , lines = lines
    , notes = Maybe.unwrap identity ln2 headers.lnobj <| ln1 <| List.map (adjustKey chartType) notes_
    , others = others
    }


adjustKey : ChartType -> Note -> Note
adjustKey ct note =
    let
        ext =
            case ct of
                Key7 ->
                    (case Bms.key note.ext of
                        6 ->
                            Bms.setKey 0

                        8 ->
                            Bms.setKey 6

                        9 ->
                            Bms.setKey 7

                        _ ->
                            identity
                    )
                        note.ext

                Key5 ->
                    (case Bms.key note.ext of
                        6 ->
                            Bms.setKey 0

                        _ ->
                            identity
                    )
                        note.ext

                Key9 ->
                    (case Bms.key note.ext of
                        38 ->
                            Bms.setKey 6

                        39 ->
                            Bms.setKey 7

                        40 ->
                            Bms.setKey 8

                        41 ->
                            Bms.setKey 9

                        8 ->
                            Bms.setKey 6

                        9 ->
                            Bms.setKey 7

                        6 ->
                            Bms.setKey 8

                        7 ->
                            Bms.setKey 9

                        _ ->
                            identity
                    )
                        note.ext

                Key14 ->
                    (case Bms.key note.ext of
                        6 ->
                            Bms.setKey 0

                        8 ->
                            Bms.setKey 6

                        9 ->
                            Bms.setKey 7

                        42 ->
                            Bms.setKey 36

                        44 ->
                            Bms.setKey 42

                        45 ->
                            Bms.setKey 43

                        _ ->
                            identity
                    )
                        note.ext

                Key10 ->
                    (case Bms.key note.ext of
                        6 ->
                            Bms.setKey 0

                        42 ->
                            Bms.setKey 36

                        _ ->
                            identity
                    )
                        note.ext
    in
    { note | ext = ext }


separeteLn : Array Float -> List Note -> List Note
separeteLn lines =
    let
        f x notes =
            case x.ext of
                Long _ l ->
                    g x.time x.measure l x notes

                _ ->
                    x :: notes

        g time measure l note notes =
            let
                measureStart =
                    Maybe.withDefault 0 (Array.get measure lines)

                measureEnd =
                    Maybe.withDefault (measureStart + TimeObject.resolution) (Array.get (measure + 1) lines)
            in
            if l > measureEnd - time then
                { time = time
                , measure = measure
                , value = note.value
                , ext = Long (Bms.key note.ext) (measureEnd - time)
                }
                    :: g (time + measureEnd - measureStart) (measure + 1) (l - (measureEnd - time)) note notes

            else
                { time = time, measure = measure, value = note.value, ext = Long (Bms.key note.ext) l } :: notes
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
                Long k l ->
                    if l == 0 then
                        case Dict.get k state of
                            Just v ->
                                ( Dict.remove k state, { note | ext = Long k (TimeObject.diff v note) } :: notes )

                            Nothing ->
                                ( Dict.insert k { time = note.time, measure = note.measure } state, note :: notes )

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
                        ( Dict.remove (Bms.key note.ext) state, { note | ext = Long (Bms.key note.ext) (TimeObject.diff v note) } :: notes )

                    Nothing ->
                        ( state, note :: notes )
    in
    List.foldr f ( Dict.empty, [] ) >> Tuple.second
