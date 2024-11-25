module List.Extra2 exposing (..)

import List.Extra as List


gatherBy : (a -> b) -> List a -> List (List a)
gatherBy f =
    List.gatherEqualsBy f >> List.map (\( a, b ) -> a :: b)


groupOn : (a -> a -> Bool) -> List a -> List (List a)
groupOn f =
    List.groupWhile f >> List.map (\( a, b ) -> a :: b)
