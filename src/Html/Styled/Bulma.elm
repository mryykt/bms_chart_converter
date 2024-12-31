module Html.Styled.Bulma exposing (file, tabs)

import Html.Styled as Html exposing (Html, a, button, div, label, li, span, ul)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Extra exposing (whenAttribute, whenJustHtml)


tabs : (a -> String) -> List a -> (a -> msg) -> a -> Html msg
tabs f ts msg t =
    div [ class "tabs" ]
        [ ul [] <|
            List.map (\tab -> li [ whenAttribute (t == tab) (class "is-active"), onClick (msg tab) ] [ a [] [ Html.text (f tab) ] ]) ts
        ]


file : String -> msg -> Maybe String -> Html msg
file t msg mname =
    div [ class "file", whenAttribute (mname /= Nothing) (class "has-name") ]
        [ label [ class "file-label" ]
            [ button [ class "file-input", onClick msg ] []
            , span [ class "file-cta" ]
                [ span [ class "file-label" ] [ Html.text t ]
                ]
            , whenJustHtml mname <|
                \name ->
                    span []
                        [ span [ class "file-name" ] [ Html.text name ]
                        ]
            ]
        ]
