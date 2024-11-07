module BMS.Convert exposing (fromRawData)

import BMS.Types exposing (..)
import BMS.Utils exposing (base)
import Dict
import String.Extra as String


fromRawData : RawBMS -> BMS
fromRawData { name, headers, data } =
    let
        ( mlens, others ) =
            List.partition ((==) 2 << .ext) data

        ( notes, others_ ) =
            List.partition (\x -> 36 <= x.ext && x.ext <= 36 * 2) others

        mlens_ =
            Dict.fromList <| List.map (\x -> ( x.measure, Maybe.withDefault 1.0 <| String.toFloat x.value )) mlens

        notes_ =
            List.map (\x -> { measure = x.measure, fraction = x.fraction, value = base 36 x.value, ext = toNoteType x.ext }) notes

        toNoteType ch =
            if 36 <= ch && ch <= 2 * 36 then
                Normal <| modBy 36 ch

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

        adjustKey ct note =
            case ct of
                Key7 ->
                    { note
                        | ext =
                            case key note.ext of
                                6 ->
                                    setKey 0 note.ext

                                8 ->
                                    setKey 6 note.ext

                                9 ->
                                    setKey 7 note.ext

                                _ ->
                                    note.ext
                    }

                Key5 ->
                    Debug.todo ""

                Key9 ->
                    Debug.todo ""
    in
    { chartType = chartType
    , header = headers
    , mlens = mlens_
    , notes = List.map (adjustKey chartType) notes_
    , others = others_
    }