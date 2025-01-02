module Bms.Converter.Options.Edit exposing (Msg, update, view)

import Bms.Converter.Clustering.KernelFunction as Kernel
import Bms.Converter.Options.Form as Form exposing (Field, Form)
import Bms.Converter.Options.Lens as Lens exposing (..)
import Bms.Types exposing (ChartType(..))
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (class)
import Html.Styled.Events exposing (onCheck, onInput)
import Html.Styled.Extra exposing (whenAttribute)
import List.Extra as List
import Maybe.Extra as Maybe


type Msg a
    = Default
    | Update (a -> a)


update : Msg Form -> Form -> Form
update msg form =
    case msg of
        Default ->
            Form.init

        Update setter ->
            setter form


view : Form -> Html (Msg Form)
view form =
    Html.div []
        [ select "chart type"
            (Dict.fromList [ ( "7-key", Key7 ), ( "5-key", Key5 ), ( "9-key", Key9 ), ( "14-key", Key14 ), ( "10-key", Key10 ) ])
            chartType
            form
        , text "number" "band width" bandWidth form
        , select "kernel function"
            (Dict.fromList
                [ ( "Gauss", Kernel.Gauss )
                , ( "Exponetail", Kernel.Exp )
                , ( "Linear", Kernel.Linear )
                , ( "Tophat", Kernel.Tophat )
                , ( "Epanechnikov", Kernel.Epanechnikov )
                ]
            )
            kernelFunction
            form
        , bool "increase scratch" { getter = .increaseScratchOptions, setter = \x y -> { y | increaseScratchOptions = x } } form
        , Html.fieldset [ Attributes.disabled <| not <| .value <| .increaseScratchOptions form ]
            [ text "number" "minimum duration" minDuration form
            , bool "include long-note" isIncludeLn form
            ]
        ]


text : String -> String -> Lens a (Field String) -> a -> Html (Msg a)
text typ label lens v =
    field label <|
        Html.input
            [ class "input"
            , whenAttribute (.invalid <| Lens.get lens v) <| class "is-danger"
            , Attributes.type_ typ
            , onInput (Update << (Lens.compose value lens).setter)
            , Attributes.value (.value <| lens.getter v)
            ]
            []


bool : String -> Lens a (Field Bool) -> a -> Html (Msg a)
bool label lens v =
    let
        lens_ =
            Lens.compose value lens
    in
    field label <|
        Html.label [ class "checkbox" ]
            [ Html.input
                [ Attributes.type_ "checkbox"
                , onCheck (Update << lens_.setter)
                , Attributes.checked (Lens.get lens_ v)
                ]
                []
            ]


select : String -> Dict String b -> Lens a (Field b) -> a -> Html (Msg a)
select label dict lens v =
    let
        option val =
            Html.option
                [ Attributes.value val
                , Attributes.selected
                    (Dict.get val dict |> Maybe.unwrap False ((==) (Lens.get lens_ v)))
                ]
                [ Html.text val ]

        get k =
            Dict.get k dict |> Maybe.withDefault (Lens.get lens_ v)

        lens_ =
            Lens.compose value lens
    in
    field label <|
        Html.div [ class "select" ]
            [ Html.select
                [ onInput (Update << lens_.setter << get)
                ]
              <|
                List.map option <|
                    Dict.keys dict
            ]


field : String -> Html msg -> Html msg
field label control =
    Html.div [ class "field is-horizontal" ]
        [ Html.div [ class "field-label" ] [ Html.label [ class "label" ] [ Html.text label ] ]
        , Html.div [ class "field-body" ] [ Html.div [ class "field" ] [ Html.p [ class "control" ] [ control ] ] ]
        ]
