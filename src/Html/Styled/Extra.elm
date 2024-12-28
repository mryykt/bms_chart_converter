module Html.Styled.Extra exposing (..)

import Html.Styled as Html exposing (Html)


whenHtml : Bool -> Html msg -> Html msg
whenHtml cond v =
    if cond then
        v

    else
        Html.div [] []


whenJustHtml : Maybe a -> (a -> Html msg) -> Html msg
whenJustHtml m f =
    case m of
        Just x ->
            f x

        _ ->
            Html.div [] []
