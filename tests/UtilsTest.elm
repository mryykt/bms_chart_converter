module UtilsTest exposing (..)

import Bms.Utils exposing (..)
import Expect
import Test exposing (..)


utilsTest : Test
utilsTest =
    describe "utils test"
        [ test "toRatio-test1" <| \_ -> Expect.equal { denominator = 2, numerator = 1 } <| toRatio (1 / 2)
        , test "toRatio-test2" <| \_ -> Expect.equal { denominator = 127, numerator = 13 } <| toRatio (13 / 127)
        , test "toRatio-test3" <| \_ -> Expect.equal { denominator = 192, numerator = 191 } <| toRatio (192 / 193)
        ]
