module Bms.Preview exposing (diffView, groupedView, view)

import Array
import Basics.Extra2 exposing (lessThan)
import Bms.Load as Load
import Bms.TimeObject as TimeObject
import Bms.Types exposing (Bms, ChartType(..), Note, NoteType(..), key, setKey)
import Bms.Utils exposing (measureLength)
import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css, id)
import List.Extra exposing (gatherEqualsBy)


type PSide
    = Left
    | Right


view : Bms -> Html msg
view bms =
    Html.div [ css [ position relative, height (vh 85), displayFlex, flexWrap wrap, flexDirection columnReverse ] ] <|
        List.map (oneMeasure False bms) <|
            fill 0 0 <|
                Load.separateByMeasure <|
                    Load.separeteLn bms.lines bms.notes


groupedView : Bms -> Html msg
groupedView bms =
    Html.div [ css [ position relative, height (vh 85), displayFlex, flexWrap wrap, flexDirection columnReverse ] ] <|
        List.map (oneMeasure True bms) <|
            fill 0 0 <|
                Load.separateByMeasure <|
                    Load.separeteLn bms.lines bms.notes


diffView : Bms -> Bms -> Html msg
diffView old new =
    let
        comp a b =
            case compare a.time b.time of
                GT ->
                    GT

                LT ->
                    LT

                EQ ->
                    compare (key a.ext) (key b.ext)

        oldChart =
            old.notes |> List.sortWith comp

        newChart =
            new.notes |> List.sortWith comp

        makeDiff oc nc =
            case ( oc, nc ) of
                ( h1 :: t1, h2 :: t2 ) ->
                    if h1.time > h2.time then
                        { h2 | value = 1 } :: makeDiff oc t2

                    else if h1.time < h2.time then
                        { h1 | value = 0 } :: makeDiff t1 nc

                    else if h1.ext == h2.ext then
                        { h1 | value = 5 } :: makeDiff t1 t2

                    else if key h1.ext > key h2.ext then
                        { h2 | value = 1 } :: makeDiff oc t2

                    else if key h1.ext < key h2.ext then
                        { h1 | value = 0 } :: makeDiff t1 nc

                    else
                        { h1 | value = 0 } :: { h2 | value = 1 } :: makeDiff t1 t2

                ( [], _ ) ->
                    List.map (\note -> { note | value = 1 }) nc

                ( _, [] ) ->
                    List.map (\note -> { note | value = 0 }) oc
    in
    groupedView { new | notes = makeDiff oldChart newChart }


oneMeasure : Bool -> Bms -> ( Int, List Note ) -> Html msg
oneMeasure isGrouped bms ( measure, notes ) =
    Html.div
        [ id <| "measure-" ++ String.fromInt measure
        , css
            [ position relative
            , height (pct <| 20 * (measureLength bms.lines measure / TimeObject.resolution))
            , width
                (pct <| laneWidth bms.chartType)
            , minWidth
                (px <| laneMinWidth bms.chartType)
            , border3 (px 1) solid (rgb 255 255 255)
            , displayFlex
            , flexDirection row
            ]
        ]
    <|
        if bms.chartType == Key14 || bms.chartType == Key10 then
            let
                ( left, right ) =
                    separateForDP notes
            in
            [ onePSide isGrouped Left bms left
            , separator
            , onePSide isGrouped Right bms right
            , measureNumber measure
            ]

        else
            [ onePSide isGrouped Left bms notes, measureNumber measure ]


onePSide : Bool -> PSide -> Bms -> List Note -> Html msg
onePSide isGrouped pSide bms notesPSide =
    let
        lanes =
            toLanes pSide bms notesPSide

        quarter n =
            Html.div [ css [ position absolute, width (pct 100), top (pct <| n * 25), border3 (px 0.5) solid (rgb 70 70 70) ] ] []
    in
    Html.div
        [ css
            [ position relative, width (pct 80), height (pct 100), backgroundColor (rgb 50 50 50), displayFlex ]
        ]
    <|
        List.map (lane isGrouped pSide bms) lanes
            ++ [ quarter 1, quarter 2, quarter 3 ]


toLanes : PSide -> Bms -> List Note -> List ( Int, List Note )
toLanes pSide bms notes =
    let
        ( minValue, maxValue ) =
            laneRange bms.chartType
    in
    gatherEqualsBy (.ext >> key) notes
        |> List.map (\( a, b ) -> ( key a.ext, a :: b ))
        |> List.sortBy Tuple.first
        |> fill minValue maxValue
        |> (if pSide == Right then
                rightScratch

            else
                identity
           )


separator : Html msg
separator =
    Html.div
        [ css
            [ position relative, width (pct 10), height (pct 100), backgroundColor (rgb 100 100 100) ]
        ]
        []


measureNumber : Int -> Html msg
measureNumber n =
    Html.div
        [ css
            [ position relative, width (pct 20), height (pct 100), backgroundColor (rgb 200 200 200) ]
        ]
        [ Html.text <| String.fromInt n ]


lane : Bool -> PSide -> Bms -> ( Int, List Note ) -> Html msg
lane isGrouped pSide bms ( k, notes ) =
    let
        w =
            noteWidth bms.chartType k

        c n =
            if isGrouped then
                groupedNoteColor n

            else
                noteColor bms.chartType k

        note n =
            Html.div
                [ css <|
                    case n.ext of
                        Normal _ ->
                            [ position absolute
                            , bottom (pct (100 * TimeObject.getFraction bms.lines n))
                            , width (pct 100)
                            , height (px 4)
                            , zIndex (int 100)
                            , backgroundColor (c n)
                            ]

                        Long _ l _ ->
                            [ position absolute
                            , bottom (pct (100 * TimeObject.getFraction bms.lines n))
                            , width (pct 100)
                            , height (pct <| 100 * l / measureLength bms.lines n.measure)
                            , zIndex (int 100)
                            , backgroundColor (c n)
                            ]
                ]
                []
    in
    Html.div
        [ css
            [ position relative
            , width (pct w)
            , whenStyle (pSide == Right && k == 0) <| float right
            , height (pct 100)
            , border3 (px 1) solid (rgb 100 100 100)
            ]
        ]
        (List.map note notes)


whenStyle : Bool -> Style -> Style
whenStyle cond style =
    if cond then
        style

    else
        batch []


separateForDP : List Note -> ( List Note, List Note )
separateForDP =
    List.partition (lessThan 36 << key << .ext) >> Tuple.mapSecond (List.map (\n -> { n | ext = setKey (key n.ext - 36) n.ext }))


rightScratch : List ( Int, List Note ) -> List ( Int, List Note )
rightScratch =
    let
        f ( i1, _ ) ( i2, _ ) =
            if i1 == 0 then
                GT

            else if i2 == 0 then
                LT

            else
                compare i1 i2
    in
    List.sortWith f


{-| maxValueをあらかじめ小さくしておくことで上限を指定していないという挙動をつくりだせるクソ仕様
-}
fill : Int -> Int -> List ( Int, List a ) -> List ( Int, List a )
fill minValue maxValue xs =
    case xs of
        [] ->
            if minValue <= maxValue then
                ( minValue, [] ) :: fill (minValue + 1) maxValue []

            else
                []

        ( x, y ) :: ys ->
            if minValue == x then
                ( minValue, y ) :: fill (minValue + 1) maxValue ys

            else
                ( minValue, [] ) :: fill (minValue + 1) maxValue xs


laneRange : ChartType -> ( Int, Int )
laneRange chartType =
    case chartType of
        Key7 ->
            ( 0, 7 )

        Key5 ->
            ( 0, 5 )

        Key9 ->
            ( 1, 9 )

        Key14 ->
            ( 0, 7 )

        Key10 ->
            ( 0, 5 )


laneWidth : ChartType -> Float
laneWidth chartType =
    case chartType of
        Key5 ->
            10

        Key14 ->
            25

        Key10 ->
            20

        _ ->
            15


laneMinWidth : ChartType -> Float
laneMinWidth chartType =
    case chartType of
        Key5 ->
            150

        Key14 ->
            300

        Key10 ->
            260

        _ ->
            220


noteWidth : ChartType -> Int -> Float
noteWidth chartType k =
    case chartType of
        Key9 ->
            100 / 9

        Key7 ->
            if k == 0 then
                16

            else
                12

        Key5 ->
            if k == 0 then
                20

            else
                15

        Key14 ->
            if k == 0 then
                16

            else
                12

        Key10 ->
            if k == 0 then
                20

            else
                15


noteColor : ChartType -> Int -> Color
noteColor chartType k =
    case chartType of
        Key9 ->
            if k == 1 || k == 9 then
                rgb 255 255 255

            else if k == 2 || k == 8 then
                rgb 255 255 0

            else if k == 3 || k == 7 then
                rgb 0 255 0

            else if k == 4 || k == 6 then
                rgb 0 0 255

            else
                rgb 255 0 0

        _ ->
            -- 7key, 5key, 14key, 10key
            if k == 0 then
                rgb 255 0 0

            else if modBy 2 k == 1 then
                rgb 255 255 255

            else
                rgb 0 100 255


groupedNoteColor : Note -> Color
groupedNoteColor note =
    let
        table =
            Array.fromList
                [ rgb 255 0 0
                , rgb 0 255 0
                , rgb 0 0 255
                , rgb 255 255 0
                , rgb 0 255 255
                , rgb 255 255 255
                , rgb 127 0 0
                , rgb 0 127 0
                , rgb 0 0 127
                , rgb 127 127 0
                , rgb 0 127 127
                ]
    in
    Array.get (modBy (Array.length table) note.value) table |> Maybe.withDefault (rgb 255 255 255)
