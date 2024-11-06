module BMS.Convert exposing (fromRawData)

import BMS.Types exposing (..)
import BMS.Utils exposing (base)
import Dict
import String.Extra as String


fromRawData : RawBMS -> BMS
fromRawData { name, headers, data } =
    let
        ( mlens, others ) =
            List.partition ((==) 2 << .channel) data

        ( notes, others_ ) =
            List.partition (\x -> 36 <= x.channel && x.channel <= 36 * 7) others

        mlens_ =
            Dict.fromList <| List.map (\x -> ( x.measure, Maybe.withDefault 1.0 <| String.toFloat x.value )) mlens

        notes_ =
            List.map (\x -> { measure = x.measure, fraction = x.fraction, value = base 36 x.value, ext = toNoteType x.channel }) notes

        toNoteType ch =
            if 36 <= ch && ch <= 2 * 36 then
                Normal <| modBy 36 ch

            else
                Debug.todo "other note type"

        chartType =
            if String.rightOfBack "." name == "pms" then
                Key9

            else if List.all ((<=) 17 << key << .ext) notes_ || String.rightOfBack "." name == "bme" then
                Key7

            else
                Key5
    in
    { chartType = chartType
    , header = headers
    , mlens = mlens_
    , notes = notes_
    , others = others_
    }
