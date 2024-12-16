module Ghost exposing (..)


type Ghost a
    = Being a
    | Ghost a


map : (a -> b) -> Ghost a -> Ghost b
map f x =
    case x of
        Being y ->
            Being (f y)

        Ghost y ->
            Ghost (f y)


get : Ghost a -> a
get x =
    case x of
        Being y ->
            y

        Ghost y ->
            y


fromBool : Ghost a -> Bool -> Ghost a
fromBool g b =
    case ( g, b ) of
        ( Being y, False ) ->
            Ghost y

        ( Ghost y, True ) ->
            Being y

        _ ->
            g


unwrap : a -> (b -> a) -> Ghost b -> a
unwrap y f x =
    case x of
        Being x_ ->
            f x_

        Ghost _ ->
            y
