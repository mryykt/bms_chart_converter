module Bms.Save exposing (save, toRawData)

import Array
import Basics.Extra exposing (..)
import Bms.TimeObject as TimeObject
import Bms.Types as Bms exposing (..)
import Bms.Utils exposing (..)
import Dict exposing (Dict)
import List.Extra as List
import List.Extra2 as List
import List.Nonempty as Nonempty exposing (ListNonempty)
import List.Nonempty.Extra as Nonempty


save : RawBms -> String
save bms =
    let
        groupedByMeasure =
            bms.data
                |> List.sortBy .measure
                |> Nonempty.groupWhileList (\a b -> a.measure == b.measure)
    in
    List.map oneMeasure groupedByMeasure |> String.join "\u{000D}\n\u{000D}\n"


oneMeasure : ListNonempty RawObject -> String
oneMeasure =
    Nonempty.sortBy .channel
        >> Nonempty.groupWhile (\a b -> a.channel == b.channel)
        >> Nonempty.map (\objs -> oneChannel (Nonempty.head objs).measure (Nonempty.head objs).channel objs)
        >> Nonempty.toList
        >> String.join "\u{000D}\n"


oneChannel : Int -> Int -> ListNonempty RawObject -> String
oneChannel measure channel =
    let
        -- TODO: 要改善
        f ls =
            case ls of
                ( obj, { denominator, numerator } ) :: ls_ ->
                    let
                        ( objs, rest ) =
                            List.partition (\( _, r ) -> modBy r.denominator denominator == 0) ls_
                                |> (\( true, false ) ->
                                        List.foldr
                                            (\o ( ts, fs ) ->
                                                if
                                                    List.all (Tuple.second >> (/=) (Tuple.second o)) ts
                                                        && Tuple.second o
                                                        /= { denominator = denominator, numerator = numerator }
                                                then
                                                    ( o :: ts, fs )

                                                else
                                                    ( ts, o :: fs )
                                            )
                                            ( [], false )
                                            true
                                   )
                    in
                    ( denominator
                    , Dict.fromList <|
                        ( numerator, obj.value )
                            :: List.map
                                (\( o, r ) ->
                                    ( r.numerator * (denominator // r.denominator), o.value )
                                )
                                objs
                    )
                        :: f (List.sortBy (Tuple.second >> .denominator >> negate) rest)

                [] ->
                    []
    in
    Nonempty.map (\obj -> ( obj, toRatio obj.fraction ))
        >> Nonempty.sortBy (Tuple.second >> .denominator >> negate)
        >> Nonempty.toList
        >> f
        >> List.map (uncurry (oneLine measure channel))
        >> String.join "\u{000D}\n"


oneLine : Int -> Int -> Int -> Dict Int String -> String
oneLine measure channel split datas =
    "#"
        ++ String.padLeft 3 '0' (String.fromInt measure)
        ++ baseToString 36 channel
        ++ ":"
        ++ String.concat (List.map (flip Dict.get datas >> Maybe.withDefault "00") (List.range 0 <| split - 1))


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
                      , channel = channel
                      }
                    , { measure = back.measure
                      , fraction = TimeObject.getFraction bms.lines back
                      , value = baseToString 36 tv
                      , channel = channel
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
