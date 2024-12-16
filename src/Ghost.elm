module Ghost exposing (..)


type Ghost a
    = Being a
    | Ghost a


unwrap : a -> (b -> a) -> Ghost b -> a
unwrap y f x =
    case x of
        Being x_ ->
            f x_

        Ghost _ ->
            y
