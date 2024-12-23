module BmsTest exposing (..)

import Bms.Load exposing (fromRawData)
import Bms.Save exposing (save, toRawData)
import Bms.Utils exposing (base)
import Dict
import Expect
import Test exposing (..)


bmsTest : Test
bmsTest =
    let
        testBms =
            { name = ""
            , headers = { bpm = 130, lnobj = Just (base 36 "zz"), waves = Dict.empty }
            , mlens = Dict.fromList [ ( 2, 0.5 ) ]
            , data =
                [ -- Normal notes(1 key)
                  { measure = 1
                  , fraction = 0
                  , value = "01"
                  , channel = 36 + 1
                  }
                , { measure = 1
                  , fraction = 0.5
                  , value = "02"
                  , channel = 36 + 1
                  }
                , { measure = 2
                  , fraction = 0
                  , value = "11"
                  , channel = 36 + 1
                  }
                , { measure = 2
                  , fraction = 0.5
                  , value = "01"
                  , channel = 36 + 1
                  }
                , { measure = 2
                  , fraction = 0.75
                  , value = "02"
                  , channel = 36 + 1
                  }

                -- long notes (channel 5x)
                , { measure = 1
                  , fraction = 0.25
                  , value = "22"
                  , channel = 5 * 36 + 2
                  }
                , { measure = 3
                  , fraction = 0
                  , value = "33"
                  , channel = 5 * 36 + 2
                  }

                -- long notes (lnobj)
                , { measure = 1
                  , fraction = 0.75
                  , value = "44"
                  , channel = 36 + 4
                  }
                , { measure = 2
                  , fraction = 0.25
                  , value = "01"
                  , channel = 36 + 5
                  }
                , { measure = 3
                  , fraction = 0.75
                  , value = "zz"
                  , channel = 36 + 4
                  }

                -- BGM
                , { measure = 4
                  , fraction = 0.5
                  , value = "55"
                  , channel = 1
                  }
                , { measure = 4
                  , fraction = 0.5
                  , value = "55"
                  , channel = 1
                  }
                ]
            }

        sort =
            List.sortWith
                (\a b ->
                    case compare a.measure b.measure of
                        GT ->
                            GT

                        LT ->
                            LT

                        EQ ->
                            compare a.fraction b.fraction
                )
    in
    describe "rawBms test"
        [ test "rawBms == toRawData (fromRawData rawBms)" <|
            \_ ->
                Expect.equal (sort testBms.data) (sort (toRawData <| fromRawData testBms).data)
        , test "save bms" <|
            \_ -> Expect.equal (save testBms) <| """#00111:0102
#00114:00000044
#00152:00220000

#00211:11000102
#00215:00010000

#00314:000000zz
#00352:33

#00401:0055
#00401:0055"""
        ]
