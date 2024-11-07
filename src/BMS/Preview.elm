module BMS.Preview exposing (view)

import BMS.Types exposing (BMS, Note, key)
import Css exposing (..)
import Dict
import Html.Styled as Html exposing (Html)
import List.Extra exposing (gatherEqualsBy)
import Svg.Styled.Attributes exposing (css, id)


view : BMS -> List ( Int, List Note ) -> Html msg
view bms notess =
    let
        oneMeasure ( measure, notes ) =
            let
                lanes =
                    fill 0 7 <| List.sortBy Tuple.first <| List.map (\( a, b ) -> ( key a.ext, a :: b )) <| gatherEqualsBy (.ext >> key) notes
            in
            Html.div
                [ id <| "measure-" ++ String.fromInt measure
                , css
                    [ position relative
                    , height (pct <| 20 * (Maybe.withDefault 1.0 <| Dict.get measure bms.mlens))
                    , width (pct 15)
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
                    List.map lane lanes
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


lane : ( Int, List Note ) -> Html msg
lane ( k, notes ) =
    let
        w =
            if k == 0 then
                16

            else
                12

        c =
            if k == 0 then
                rgb 255 0 0

            else if modBy 2 k == 1 then
                rgb 255 255 255

            else
                rgb 0 100 255

        note n =
            Html.div
                [ css
                    [ position absolute
                    , bottom (pct (100 * n.fraction))
                    , width (pct 100)
                    , height (px 4)
                    , backgroundColor c
                    ]
                ]
                []
    in
    Html.div
        [ css
            [ position relative
            , width (pct w)
            , height (pct 100)
            , border3 (px 1) solid (rgb 100 100 100)
            ]
        ]
    <|
        List.map note notes


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
