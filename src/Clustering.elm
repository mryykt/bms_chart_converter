module Clustering exposing (..)

import BTime
import Bms.Types exposing (Note)
import List.Extra as List
import List.Extra2 as List


rough : List Note -> List (List Note)
rough notes =
    List.groupOn (\a b -> BTime.diff b a < 1) notes


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
