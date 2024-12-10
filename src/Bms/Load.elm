module Bms.Load exposing (fromRawData, separateByMeasure, separeteLn)

import BTime
import Bms.Types as Bms exposing (Bms, ChartType(..), Note, NoteType(..), Object, RawBms)
import Bms.Utils exposing (base)
import Dict exposing (Dict)
import List.Extra as List
import Maybe.Extra as Maybe
import String.Extra as String


fromRawData : RawBms -> Bms
fromRawData { name, headers, mlens, data } =
    let
        ( notes, others ) =
            List.partition (\x -> 36 <= x.ext && x.ext < 36 * 3 || 5 * 36 <= x.ext && x.ext < 7 * 36) data

        notes_ =
            List.map (\x -> { measure = x.measure, fraction = x.fraction, value = base 36 x.value, ext = toNoteType x.ext }) notes

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
    , mlens = mlens
    , notes = Maybe.unwrap identity (ln2 mlens) headers.lnobj <| ln1 mlens <| List.map (adjustKey chartType) notes_
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


separeteLn : List Note -> List Note
separeteLn =
    let
        f x notes =
            case x.ext of
                Long _ l ->
                    g x.measure x.fraction l x notes

                _ ->
                    x :: notes

        g m fr l note notes =
            if l > 1.0 then
                { note | measure = m, fraction = fr, ext = Long (Bms.key note.ext) (1.0 - fr) } :: g (m + 1) 0.0 (l - 1.0) note notes

            else
                { note | measure = m, fraction = fr, ext = Long (Bms.key note.ext) (l - fr) } :: notes
    in
    List.foldr f [] >> Bms.sort


separateByMeasure : List (Object x v) -> List ( Int, List (Object x v) )
separateByMeasure =
    List.groupWhile (\a b -> a.measure == b.measure) >> List.map (\( a, b ) -> ( a.measure, a :: b ))


ln1 : Dict Int Float -> List Note -> List Note
ln1 mlens =
    let
        f note ( state, notes ) =
            case note.ext of
                Long k l ->
                    if l == 0 then
                        case Dict.get k state of
                            Just v ->
                                ( Dict.remove k state, { note | ext = Long k (BTime.diffWithMeasureLength mlens v note) } :: notes )

                            Nothing ->
                                ( Dict.insert k { measure = note.measure, fraction = note.fraction } state, note :: notes )

                    else
                        ( state, note :: notes )

                _ ->
                    ( state, note :: notes )
    in
    List.foldr f ( Dict.empty, [] ) >> Tuple.second


ln2 : Dict Int Float -> Int -> List Note -> List Note
ln2 mlens lnobj =
    let
        f note ( state, notes ) =
            if note.value == lnobj then
                ( Dict.insert (Bms.key note.ext) { measure = note.measure, fraction = note.fraction } state, notes )

            else
                case Dict.get (Bms.key note.ext) state of
                    Just v ->
                        ( Dict.remove (Bms.key note.ext) state, { note | ext = Long (Bms.key note.ext) (BTime.diffWithMeasureLength mlens v note) } :: notes )

                    Nothing ->
                        ( state, note :: notes )
    in
    List.foldr f ( Dict.empty, [] ) >> Tuple.second
