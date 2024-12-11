module TimeObjectTest exposing (..)

import Array
import Bms.TimeObject as TimeObject
import Expect
import Test exposing (..)


timeObjectTest : Test
timeObjectTest =
    describe "TimeObject module test"
        [ test "to time test-1" <| \_ -> Expect.within (Expect.Absolute 0.001) (TimeObject.fromMeasureAndFraction (Array.fromList [ 0, 960, 1920 ]) 1 0.5) 1440
        , test "to time test-2" <| \_ -> Expect.within (Expect.Absolute 0.001) (TimeObject.fromMeasureAndFraction (Array.fromList [ 0, 960, 1440 ]) 1 0.5) 1200
        , test "to time test-3" <| \_ -> Expect.within (Expect.Absolute 0.001) (TimeObject.fromMeasureAndFraction (Array.fromList [ 0, 960, 1440 ]) 2 0.5) 1920
        , test "from time test-1" <| \_ -> Expect.equal (TimeObject.getFraction (Array.fromList [ 0, 960, 1920 ]) { time = 1440, measure = 1 }) 0.5
        , test "from time test-2" <| \_ -> Expect.equal (TimeObject.getFraction (Array.fromList [ 0, 960, 1440 ]) { time = 1200, measure = 1 }) 0.5
        , test "from time test-3" <| \_ -> Expect.equal (TimeObject.getFraction (Array.fromList [ 0, 960, 1920 ]) { time = 480, measure = 0 }) 0.5
        , test "from time test-4" <| \_ -> Expect.equal (TimeObject.getFraction (Array.fromList [ 0, 960, 1920 ]) { time = 2400, measure = 2 }) 0.5
        ]
