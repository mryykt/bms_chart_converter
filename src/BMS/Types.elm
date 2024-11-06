module BMS.Types exposing (..)

import BMS.Utils exposing (base)
import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Tuple


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


type alias OtheObject =
    Object () String


type alias RawBMS =
    { headers : Headers
    , data : List RawData
    }


type alias Headers =
    { bpm : Float
    , waves : Dict Int String
    }


type alias RawData =
    { measure : Int
    , fraction : Float
    , value : String
    , channel : Int
    }


decodeRawBMS : Decoder RawBMS
decodeRawBMS =
    D.map2 RawBMS (D.field "header" decodeHeaders) (D.field "data" (D.list decodeRawData))


decodeHeaders : Decoder Headers
decodeHeaders =
    let
        f =
            Tuple.mapFirst <| base 36
    in
    D.map2 Headers (D.field "bpm" D.float) (D.field "waves" (D.map (Dict.fromList << List.map f) (D.keyValuePairs D.string)))


decodeRawData : Decoder RawData
decodeRawData =
    D.map4 RawData
        (D.field "measure" D.int)
        (D.field "fraction" D.float)
        (D.field "value" D.string)
        (D.field "channel" <| D.map (base 36) D.string)
