module Bms.Utils exposing (..)

import Basics.Extra exposing (..)
import Bms.TimeObject as TimeObject
import Bms.Types exposing (ChartType(..), Note, NoteType(..), Object)
import List.Extra as List
import List.Extra2 as List
import List.Nonempty as Nonempty exposing (ListNonempty)
import List.Nonempty.Extra as Nonempty
import Maybe.Extra as Maybe


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


sort : List (Object x v) -> List (Object x v)
sort =
    List.sortBy .time


adjustKey : ChartType -> Int -> Int
adjustKey ct k =
    let
        get n dict =
            List.find (Tuple.first >> (==) n) dict |> Maybe.unwrap n Tuple.second
    in
    case ct of
        Key7 ->
            get k key7

        Key5 ->
            get k key5

        Key9 ->
            get k key9

        Key14 ->
            get k key14

        Key10 ->
            get k key10


reverseAdjustKey : ChartType -> Int -> Int
reverseAdjustKey ct k =
    let
        get n dict =
            List.find (Tuple.second >> (==) n) dict |> Maybe.unwrap n Tuple.first
    in
    case ct of
        Key7 ->
            get k key7

        Key5 ->
            get k key5

        Key9 ->
            get k key9

        Key14 ->
            get k key14

        Key10 ->
            get k key10


key7 : List ( Int, Int )
key7 =
    [ ( 6, 0 ), ( 8, 6 ), ( 9, 7 ) ]


key5 : List ( Int, Int )
key5 =
    [ ( 6, 0 ) ]


key9 : List ( Int, Int )
key9 =
    [ ( 38, 6 ), ( 39, 7 ), ( 40, 8 ), ( 41, 9 ), ( 8, 6 ), ( 9, 7 ), ( 6, 8 ), ( 7, 9 ) ]


key14 : List ( Int, Int )
key14 =
    key7 ++ [ ( 42, 36 ), ( 44, 42 ), ( 45, 43 ) ]


key10 : List ( Int, Int )
key10 =
    key5 ++ [ ( 42, 36 ) ]


isLn : NoteType -> Bool
isLn nt =
    case nt of
        Long _ _ _ ->
            True

        _ ->
            False


doujiOshi : ListNonempty Note -> Bool
doujiOshi =
    Nonempty.toList >> List.foldl2 (\x y acc -> acc || Maybe.unwrap False (.time >> (==) x.time) y) False


minDurationOfGroup : ListNonempty Note -> Float
minDurationOfGroup =
    Nonempty.toList >> List.foldl2 (\x y acc -> Maybe.unwrap acc (flip TimeObject.diff x >> min acc) y) 1000


groupLength : ListNonempty Note -> Float
groupLength notes =
    groupRange notes |> uncurry (flip (-))


isOverlappingGroup : ListNonempty Note -> ListNonempty Note -> Bool
isOverlappingGroup notes1 notes2 =
    let
        ( h1, t1 ) =
            groupRange notes1

        ( h2, t2 ) =
            groupRange notes2
    in
    not (h1 > t2 || h2 > t1)


groupRange : ListNonempty Note -> ( Float, Float )
groupRange =
    let
        lastTimeOfNote note =
            case note.ext of
                Long _ l _ ->
                    note.time + l

                _ ->
                    note.time
    in
    Nonempty.foldl (\note -> Tuple.mapBoth (min note.time) (max (lastTimeOfNote note))) ( 999999, 0 )
