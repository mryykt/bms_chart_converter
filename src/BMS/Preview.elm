module BMS.Preview exposing (view)

import BMS.Types exposing (BMS, ChartType(..), Note, NoteType(..), key, setKey)
import Css exposing (..)
import Dict
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css, id)
import List.Extra exposing (gatherEqualsBy)


type PSide
    = Left
    | Right


view : BMS -> List ( Int, List Note ) -> Html msg
view bms notess =
    let
        oneMeasure ( measure, notes ) =
            let
                lanes =
                    let
                        ( minValue, maxValue ) =
                            case bms.chartType of
                                Key7 ->
                                    ( 0, 7 )

                                Key5 ->
                                    ( 0, 5 )

                                Key9 ->
                                    ( 1, 9 )

                                Key14 ->
                                    Debug.todo ""
                    in
                    fill minValue maxValue <| List.sortBy Tuple.first <| List.map (\( a, b ) -> ( key a.ext, a :: b )) <| gatherEqualsBy (.ext >> key) notes
            in
            Html.div
                [ id <| "measure-" ++ String.fromInt measure
                , css
                    [ position relative
                    , height (pct <| 20 * (Maybe.withDefault 1.0 <| Dict.get measure bms.mlens))
                    , width
                        (pct
                            (if bms.chartType == Key5 then
                                10

                             else
                                15
                            )
                        )
                    , minWidth (px 150)
                    , padding2 zero (px 5)
                    , border3 (px 1) solid (rgb 255 255 255)
                    , displayFlex
                    , flexDirection row
                    ]
                ]
                [ Html.div
                    [ css
                        [ position relative
                        , width (pct 80)
                        , height (pct 100)
                        , backgroundColor (rgb 50 50 50)
                        , displayFlex
                        ]
                    ]
                  <|
                    List.map (lane Left bms.chartType) lanes
                , Html.div
                    [ css
                        [ position relative
                        , width (pct 20)
                        , height (pct 100)
                        , backgroundColor (rgb 200 200 200)
                        ]
                    ]
                    [ Html.text <| String.fromInt measure ]
                ]
    in
    Html.div [ css [ position relative, height (vh 90), displayFlex, flexWrap wrap, flexDirection columnReverse ] ] <| List.map oneMeasure <| fill 0 0 notess


lane : PSide -> ChartType -> ( Int, List Note ) -> Html msg
lane pside chartType ( k, notes ) =
    let
        w =
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
                    Debug.todo ""

        c =
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
                    -- 7key, 5key
                    if k == 0 then
                        rgb 255 0 0

                    else if modBy 2 k == 1 then
                        rgb 255 255 255

                    else
                        rgb 0 100 255

        note n =
            Html.div
                [ css <|
                    case n.ext of
                        Normal _ ->
                            [ position absolute
                            , bottom (pct (100 * n.fraction))
                            , width (pct 100)
                            , height
                                (px 4)
                            , backgroundColor c
                            ]

                        Long _ l ->
                            [ position absolute
                            , bottom (pct (100 * n.fraction))
                            , width (pct 100)
                            , height
                                (pct <| 100 * l)
                            , backgroundColor c
                            ]
                ]
                []
    in
    Html.div
        [ css
            [ position relative
            , width (pct w)
            , whenStyle (pside == Right && k == 0) <| right (px 0)
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
    List.partition ((>) 36 << key << .ext) >> Tuple.mapSecond (List.map (\n -> { n | ext = setKey (key n.ext - 36) n.ext }))


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
