module Bms.Converter.Clustering.KernelFunction exposing (..)


gauss : Float -> Float
gauss x =
    1 / sqrt (2 * pi) * e ^ (-x * x / 2)


tophat : Float -> Float
tophat x =
    if -1 < x && x < 1 then
        0.5

    else
        0


linear : Float -> Float
linear x =
    if abs x < 1 then
        1 - abs x

    else
        0


exp : Float -> Float
exp x =
    0.5 * e ^ -(abs x)


epanechnikov : Float -> Float
epanechnikov x =
    if abs x < sqrt 5 then
        3 / (4 * sqrt 5) * (1 - x ^ 2 / 5)

    else
        0
