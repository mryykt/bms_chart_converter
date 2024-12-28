module Html.Styled.Bulma exposing (..)

import Html.Styled exposing (Html, button, div, label, span, text)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Extra exposing (whenAttribute, whenJustHtml)


file : String -> msg -> Maybe String -> Html msg
file t msg mname =
    div [ class "file", whenAttribute (mname /= Nothing) (class "has-name") ]
        [ label [ class "file-label" ]
            [ button [ class "file-input", onClick msg ] []
            , span [ class "file-cta" ]
                [ span [ class "file-label" ] [ text t ]
                ]
            , whenJustHtml mname <|
                \name ->
                    span []
                        [ span [ class "file-name" ] [ text name ]
                        ]
            ]
        ]
