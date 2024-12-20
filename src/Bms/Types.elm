module Bms.Types exposing (..)

import Array exposing (Array)
import Bms.TimeObject exposing (TimeObject)
import Bms.Utils exposing (base)
import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Tuple


type alias Bms =
    { chartType : ChartType
    , header : Headers
    , lines : Array Float
    , notes : List Note
    , others : List RawObject
    }


type ChartType
    = Key7
    | Key5
    | Key9
    | Key14
    | Key10


type alias Object x v =
    TimeObject
        { value : v
        , ext : x
        }


type alias Note =
    Object NoteType Int


type NoteType
    = Normal Int
    | Long Int Float Int


key : NoteType -> Int
key nt =
    case nt of
        Normal k ->
            k

        Long k _ _ ->
            k


setKey : Int -> NoteType -> NoteType
setKey k nt =
    case nt of
        Normal _ ->
            Normal k

        Long _ l t ->
            Long k l t


type alias RawBms =
    { name : String
    , headers : Headers
    , mlens : Dict Int Float
    , data : List RawObject
    }


type alias RawObject =
    { measure : Int
    , fraction : Float
    , value : String
    , channel : Int
    }


type alias Headers =
    { bpm : Float
    , lnobj : Maybe Int
    , waves : Dict Int String
    }


decodeRawBms : Decoder RawBms
decodeRawBms =
    let
        rawComp a b =
            case compare a.measure b.measure of
                LT ->
                    LT

                GT ->
                    GT

                EQ ->
                    compare a.fraction b.fraction
    in
    D.map4 RawBms
        (D.field "name" D.string)
        (D.field "header" decodeHeaders)
        (D.field "mlens" <| D.map (Dict.fromList << List.map (Tuple.mapFirst (Maybe.withDefault -1 << String.toInt))) (D.keyValuePairs D.float))
        (D.field "data" (D.map (List.sortWith rawComp) <| D.list decodeRawObject))


decodeHeaders : Decoder Headers
decodeHeaders =
    let
        f =
            Tuple.mapFirst <| base 36
    in
    D.map3 Headers
        (D.field "bpm" D.float)
        (D.field "lnobj" <| D.map (Maybe.map (base 36)) <| D.maybe D.string)
        (D.field "waves" (D.map (Dict.fromList << List.map f) (D.keyValuePairs D.string)))


decodeRawObject : Decoder RawObject
decodeRawObject =
    D.map4 RawObject
        (D.field "measure" D.int)
        (D.field "fraction" D.float)
        (D.field "value" D.string)
        (D.field "channel" <| D.map (base 36) D.string)


sort : List (Object x v) -> List (Object x v)
sort =
    List.sortBy .time
