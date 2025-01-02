module Bms.Types exposing
    ( Bms
    , ChartType(..)
    , Note
    , NoteType(..)
    , Object
    , RawBms
    , RawHeader
    , RawObject
    , decodeRawBms
    , defRawBms
    )

import Array exposing (Array)
import Bms.TimeObject exposing (TimeObject)
import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import List.Extra as List
import Maybe.Extra as Maybe
import String.Extra2 as String
import Tuple


type alias Bms =
    { name : String
    , chartType : ChartType
    , header : RawHeader
    , bpm : Float
    , lnobj : Maybe Int
    , waves : Dict Int String
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


type alias RawBms =
    { name : String
    , header : RawHeader
    , mlens : Dict Int Float
    , data : List RawObject
    }


type alias RawHeader =
    Dict String (List String)


type alias RawObject =
    { measure : Int
    , fraction : Float
    , value : String
    , channel : Int
    }


defRawBms : RawBms
defRawBms =
    let
        note k =
            { measure = 1, fraction = 0, value = "00", channel = 36 + k }
    in
    { name = "def.bme"
    , header = Dict.fromList [ ( "wav00", [ "0.wav" ] ) ]
    , mlens = Dict.empty
    , data = [ note 1, note 2, note 3, note 4, note 5, note 6, note 8, note 9 ]
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
        (D.field "header" <| D.dict (D.map (Maybe.withDefault []) <| D.maybe <| D.list D.string))
        (D.field "mlens" <| D.map (Dict.fromList << List.map (Tuple.mapFirst (Maybe.withDefault -1 << String.toInt))) (D.keyValuePairs D.float))
        (D.field "data" (D.map (List.sortWith rawComp) <| D.list decodeRawObject))


decodeRawObject : Decoder RawObject
decodeRawObject =
    D.map4 RawObject
        (D.field "measure" D.int)
        (D.field "fraction" D.float)
        (D.field "value" D.string)
        (D.field "channel" <| D.map (String.baseFromString 36) D.string)
