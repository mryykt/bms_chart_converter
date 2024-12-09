module BTimeTest exposing (..)

import BTime
import Dict
import Expect
import Test exposing (..)


bTimeTest : Test
bTimeTest =
    describe "BTime module test"
        [ describe "diffWithMeasureLength"
            [ test "test1" <| \_ -> Expect.within (Expect.Absolute 0.00001) (BTime.diffWithMeasureLength Dict.empty { measure = 3, fraction = 0.5 } { measure = 1, fraction = 0.75 }) 1.75
            , test "test2" <| \_ -> Expect.within (Expect.Absolute 0.00001) (BTime.diffWithMeasureLength (Dict.fromList [ ( 2, 0.5 ) ]) { measure = 3, fraction = 0.5 } { measure = 1, fraction = 0.75 }) 1.25
            ]
        ]
