module List.Extra2 exposing (..)

import List.Extra as List


gatherBy : (a -> b) -> List a -> List (List a)
gatherBy f =
    List.gatherEqualsBy f >> List.map (\( a, b ) -> a :: b)


groupOn : (a -> a -> Bool) -> List a -> List (List a)
groupOn f =
    List.groupWhile f >> List.map (\( a, b ) -> a :: b)


foldl2 : (a -> Maybe a -> b -> b) -> b -> List a -> b
foldl2 f init =
    let
        helper acc xs =
            case xs of
                x :: y :: ys ->
                    helper (f x (Just y) acc) (y :: ys)

                x :: [] ->
                    f x Nothing acc

                [] ->
                    acc
    in
    helper init
