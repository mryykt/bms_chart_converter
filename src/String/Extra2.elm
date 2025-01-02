module String.Extra2 exposing (..)


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
    String.foldl f 0


{-| 2桁のものしか考えない
-}
baseToString : Int -> Int -> String
baseToString n x =
    let
        f d =
            if 0 <= d && d <= 9 then
                String.fromInt d

            else
                String.fromChar <| Char.fromCode <| Char.toCode 'a' + (d - 10)
    in
    f (x // n) ++ f (modBy n x)
