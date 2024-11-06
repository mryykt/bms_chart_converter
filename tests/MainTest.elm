module MainTest exposing (..)

import Expect
import Fuzz exposing (string)
import Main exposing (Msg(..), update)
import Test exposing (..)


updateTest : Test
updateTest =
    describe "update test"
        []
