module Clustering exposing (..)

import Chart as C
import Chart.Attributes as CA
import Html exposing (Html)
import List.Extra as List


clustering : (a -> Float) -> List a -> List (List a)
clustering f =
    Debug.todo ""


density : (Float -> Float) -> Float -> List Float -> Float -> Float
density kernel bandwidth xs x =
    let
        n =
            toFloat <| List.length xs
    in
    1 / (n * bandwidth) * List.sum (List.map (\xi -> kernel <| (x - xi) / bandwidth) xs)


testView : Float -> Float -> (Float -> Float) -> Html msg
testView mint maxt f =
    let
        data =
            List.map (\t -> { t = t, y = f t }) <| List.map (toFloat >> (*) 0.1 >> (+) mint) <| List.range 0 (ceiling (maxt - mint))
    in
    C.chart [ CA.width 300, CA.height 300, CA.padding { top = 30, bottom = 10, left = 10, right = 10 } ]
        [ C.xLabels []
        , C.yLabels []
        , C.series .t
            [ C.interpolated .y [ CA.monotone ] [ CA.circle ]
            ]
            data
        ]
