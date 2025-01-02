module Html.Styled.Extra exposing (..)

import Basics.Extra2 exposing (ifelse, just)
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes exposing (css)


whenHtml : Bool -> Html msg -> Html msg
whenHtml cond v =
    ifelse cond v (Html.div [] [])


whenJustHtml : Maybe a -> (a -> Html msg) -> Html msg
whenJustHtml m f =
    just m f (Html.div [] [])


whenAttribute : Bool -> Attribute msg -> Attribute msg
whenAttribute cond v =
    ifelse cond v (css [])


whenJustAttribute : Maybe a -> (a -> Attribute msg) -> Attribute msg
whenJustAttribute m f =
    just m f (css [])
