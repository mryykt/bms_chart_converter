module BMS.Convert exposing (fromRawData, separateByMeasure, separeteLn)

import BMS.Types as BMS exposing (..)
import BMS.Utils exposing (base)
import Dict
import List.Extra as List
import Maybe.Extra as Maybe
import String.Extra as String


fromRawData : RawBMS -> BMS
fromRawData { name, headers, mlens, data } =
    let
        ( notes, others ) =
            List.partition (\x -> 36 <= x.ext && x.ext < 36 * 3) data

        notes_ =
            List.map (\x -> { measure = x.measure, fraction = x.fraction, value = base 36 x.value, ext = toNoteType x.ext }) notes

        toNoteType ch =
            if 36 <= ch && ch < 3 * 36 then
                Normal <| ch - 36

            else
                Debug.todo "other note type"

        chartType =
            if String.rightOfBack "." name == "pms" then
                Key9
                -- とりあえず，5keyのフリーゾーンを無視する

            else if List.any ((<=) 7 << key << .ext) notes_ || String.rightOfBack "." name == "bme" then
                Key7

            else
                Key5
    in
    { chartType = chartType
    , header = headers
    , mlens = mlens
    , notes = Maybe.unwrap identity ln headers.lnobj <| List.map (adjustKey chartType) notes_
    , others = others
    }


adjustKey : ChartType -> Note -> Note
adjustKey ct note =
    case ct of
        Key7 ->
            { note
                | ext =
                    note.ext
                        |> (case key note.ext of
                                6 ->
                                    setKey 0

                                8 ->
                                    setKey 6

                                9 ->
                                    setKey 7

                                _ ->
                                    identity
                           )
            }

        Key5 ->
            { note
                | ext =
                    note.ext
                        |> (case key note.ext of
                                6 ->
                                    setKey 0

                                _ ->
                                    identity
                           )
            }

        Key9 ->
            { note
                | ext =
                    note.ext
                        |> (case key note.ext of
                                38 ->
                                    setKey 6

                                39 ->
                                    setKey 7

                                40 ->
                                    setKey 8

                                41 ->
                                    setKey 9

                                8 ->
                                    setKey 6

                                9 ->
                                    setKey 7

                                6 ->
                                    setKey 8

                                7 ->
                                    setKey 9

                                _ ->
                                    identity
                           )
            }


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
                { note | measure = m, fraction = fr, ext = Long (key note.ext) (1.0 - fr) } :: g (m + 1) 0.0 (l - 1.0) note notes

            else
                { note | measure = m, fraction = fr, ext = Long (key note.ext) (l - fr) } :: notes
    in
    List.foldr f [] >> BMS.sort


separateByMeasure : List (Object x v) -> List ( Int, List (Object x v) )
separateByMeasure =
    List.groupWhile (\a b -> a.measure == b.measure) >> List.map (\( a, b ) -> ( a.measure, a :: b ))


ln : Int -> List Note -> List Note
ln lnobj =
    let
        f note ( state, notes ) =
            if note.value == lnobj then
                ( Dict.insert (key note.ext) { measure = note.measure, fraction = note.fraction } state, notes )

            else
                case Dict.get (key note.ext) state of
                    Just v ->
                        ( Dict.remove (key note.ext) state, { note | ext = Long (key note.ext) (toFloat (v.measure - note.measure) + v.fraction - note.fraction) } :: notes )

                    Nothing ->
                        ( state, note :: notes )
    in
    List.foldr f ( Dict.empty, [] ) >> Tuple.second
