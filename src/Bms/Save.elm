module Bms.Save exposing (..)

import Array
import Bms.TimeObject as TimeObject
import Bms.Types as Bms exposing (..)
import Bms.Utils exposing (..)
import Dict
import List.Extra as List


toRawData : Bms -> RawBms
toRawData bms =
    let
        mlens =
            Array.toList bms.lines
                |> List.tail
                |> Maybe.withDefault []
                |> List.mapAccuml (\acc x -> ( x, (x - acc) / TimeObject.resolution )) 0
                |> Tuple.second
                |> List.indexedMap Tuple.pair
                |> List.filter (Tuple.second >> (/=) 1.0)
                |> Dict.fromList

        fromNote : Note -> List RawObject
        fromNote note =
            case note.ext of
                Normal n ->
                    [ { measure = note.measure
                      , fraction = TimeObject.getFraction bms.lines note
                      , value = baseToString 36 note.value
                      , channel =
                            Bms.reverseAdjustKey bms.chartType n
                                + 36
                      }
                    ]

                Long n l tv ->
                    let
                        channel =
                            Bms.reverseAdjustKey bms.chartType n
                                + (if Just tv == bms.header.lnobj then
                                    36

                                   else
                                    5 * 36
                                  )

                        back =
                            TimeObject.add bms.lines { note | time = note.time } l
                    in
                    [ { measure = note.measure
                      , fraction = TimeObject.getFraction bms.lines note
                      , value = baseToString 36 note.value
                      , channel =
                            channel
                      }
                    , { measure = back.measure
                      , fraction = TimeObject.getFraction bms.lines back
                      , value = baseToString 36 tv
                      , channel =
                            channel
                      }
                    ]

        data =
            bms.others ++ List.concatMap fromNote bms.notes
    in
    { name = ""
    , headers = bms.header
    , mlens = mlens
    , data = data
    }
