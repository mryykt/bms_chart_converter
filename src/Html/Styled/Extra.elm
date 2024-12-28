module Html.Styled.Extra exposing (..)

import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes exposing (css)


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


whenAttribute : Bool -> Attribute msg -> Attribute msg
whenAttribute cond v =
    if cond then
        v

    else
        css []


whenJustAttribute : Maybe a -> (a -> Attribute msg) -> Attribute msg
whenJustAttribute m f =
    case m of
        Just x ->
            f x

        _ ->
            css []
