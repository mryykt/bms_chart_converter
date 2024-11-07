module BMS.Types exposing (..)

import BMS.Utils exposing (base)
import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Tuple


type alias BMS =
    { chartType : ChartType
    , header : Headers
    , mlens : Dict Int Float
    , notes : List Note
    , others : List RawData
    }


type ChartType
    = Key7
    | Key5
    | Key9


type alias Object x v =
    { measure : Int
    , fraction : Float
    , value : v
    , ext : x
    }


type alias Note =
    Object NoteType Int


type NoteType
    = Normal Int
    | Long Int Float


key : NoteType -> Int
key nt =
    case nt of
        Normal k ->
            k

        Long k _ ->
            k


setKey : Int -> NoteType -> NoteType
setKey k nt =
    case nt of
        Normal _ ->
            Normal k

        Long _ l ->
            Long k l


type alias RawBMS =
    { name : String
    , headers : Headers
    , mlens : Dict Int Float
    , data : List RawData
    }


type alias RawData =
    Object Int String


type alias Headers =
    { bpm : Float
    , waves : Dict Int String
    }


decodeRawBMS : Decoder RawBMS
decodeRawBMS =
    let
        comp a b =
            case compare a.measure b.measure of
                LT ->
                    LT

                GT ->
                    GT

                EQ ->
                    compare a.fraction b.fraction
    in
    D.map4 RawBMS
        (D.field "name" D.string)
        (D.field "header" decodeHeaders)
        (D.field "mlens" <| D.map (Dict.fromList << List.map (Tuple.mapFirst (Maybe.withDefault -1 << String.toInt))) (D.keyValuePairs D.float))
        (D.field "data" (D.map (List.sortWith comp) <| D.list decodeRawData))


decodeHeaders : Decoder Headers
decodeHeaders =
    let
        f =
            Tuple.mapFirst <| base 36
    in
    D.map2 Headers (D.field "bpm" D.float) (D.field "waves" (D.map (Dict.fromList << List.map f) (D.keyValuePairs D.string)))


decodeRawData : Decoder RawData
decodeRawData =
    D.map4 Object
        (D.field "measure" D.int)
        (D.field "fraction" D.float)
        (D.field "value" D.string)
        (D.field "channel" <| D.map (base 36) D.string)
