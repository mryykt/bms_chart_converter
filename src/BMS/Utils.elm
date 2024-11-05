module BMS.Utils exposing (..)


base : Int -> String -> Int
base n =
    let
        f c a =
            let
                code =
                    Char.toCode c
            in
            n
                * a
                + (if Char.toCode '0' <= code && code <= Char.toCode '9' then
                    code - Char.toCode '0'

                   else if Char.toCode 'a' <= code && code <= Char.toCode 'z' then
                    code - Char.toCode 'a' + 10

                   else
                    code - Char.toCode 'A' + 10
                  )
    in
    String.foldr f 0
