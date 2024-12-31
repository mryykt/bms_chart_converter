module Bms.Converter.Options.Edit exposing (..)

import Bms.Converter.Clustering.KernelFunction as Kernel
import Bms.Converter.Options exposing (Optional, Options, defOptions)
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (class)
import Html.Styled.Events exposing (onCheck, onInput)
import List.Extra as List
import Maybe.Extra as Maybe


type Msg a
    = Default
    | Update (a -> a)


type alias Getter a b =
    a -> b


type alias Setter a b =
    b -> a -> a


type alias Lens a b =
    { getter : Getter a b, setter : Setter a b }


update : Msg Options -> Options -> Options
update msg options =
    case msg of
        Default ->
            defOptions

        Update setter ->
            setter options


view : Options -> Html (Msg Options)
view options =
    Html.div []
        [ float "band width" { getter = .bandWidth, setter = \x y -> { y | bandWidth = x } } options
        , select "kernel function"
            (Dict.fromList
                [ ( "Gauss", Kernel.Gauss )
                , ( "Linear", Kernel.Linear )
                , ( "Exp", Kernel.Exp )
                , ( "Tophat", Kernel.Tophat )
                , ( "Epanechnikov", Kernel.Epanechnikov )
                ]
            )
            { getter = .kernelFunction, setter = \x y -> { y | kernelFunction = x } }
            options
        , optional "increase scratch"
            { getter = .inscreaseScratchOptions, setter = \x y -> { y | inscreaseScratchOptions = x } }
            options
            [ int "min duration" { getter = .minDuration, setter = \x y -> { y | minDuration = x } }
            , bool "including long-note" { getter = .isIncludingLn, setter = \x y -> { y | isIncludingLn = x } }
            ]
        ]


optional : String -> Lens a (Optional b) -> a -> List (b -> Html (Msg b)) -> Html (Msg a)
optional l { getter, setter } v forms =
    let
        f : Msg b -> Msg a
        f msg =
            case msg of
                Default ->
                    Default

                Update setter_ ->
                    Update
                        (setter
                            (let
                                y =
                                    getter v
                             in
                             { y | value = setter_ y.value }
                            )
                        )
    in
    Html.div
        []
        [ checkbox_ l
            (getter v).enabled
            (\b ->
                Update
                    (\y ->
                        setter
                            (let
                                y_ =
                                    getter y
                             in
                             { y_ | enabled = b }
                            )
                            y
                    )
            )
        , Html.map f <|
            (\opt ->
                Html.fieldset [ Attributes.disabled <| not opt.enabled ] <| List.map ((|>) opt.value) forms
            )
            <|
                getter v
        ]


text : String -> Lens a String -> a -> Html (Msg a)
text l { getter, setter } v =
    field l <|
        Html.input
            [ class "input", onInput (Update << setter), Attributes.value (getter v) ]
            []


int : String -> Lens a Int -> a -> Html (Msg a)
int l { getter, setter } =
    text l
        { getter = getter >> String.fromInt
        , setter = setter << Maybe.withDefault 0 << String.toInt
        }


float : String -> Lens a Float -> a -> Html (Msg a)
float l { getter, setter } =
    text l
        { getter = getter >> String.fromFloat
        , setter = setter << Maybe.withDefault 0 << String.toFloat
        }


bool : String -> Lens a Bool -> a -> Html (Msg a)
bool l { getter, setter } v =
    checkbox_ l (getter v) (Update << setter)


checkbox_ : String -> Bool -> (Bool -> msg) -> Html msg
checkbox_ label checked msg =
    field label <| Html.label [ class "checkbox" ] [ Html.input [ Attributes.type_ "checkbox", onCheck msg, Attributes.checked checked ] [] ]


select : String -> Dict String b -> Lens a b -> a -> Html (Msg a)
select l dict { getter, setter } v =
    let
        option val =
            Html.option [ Attributes.value val, Attributes.selected (Dict.get val dict |> Maybe.unwrap False ((==) (getter v))) ] [ Html.text val ]

        get k =
            Dict.get k dict |> Maybe.withDefault (getter v)
    in
    field l <|
        Html.div [ class "select" ]
            [ Html.select
                [ onInput (Update << setter << get)
                ]
              <|
                List.map option <|
                    Dict.keys dict
            ]


field : String -> Html msg -> Html msg
field l control =
    Html.div [ class "field is-horizontal" ]
        [ Html.div [ class "field-label" ] [ Html.label [ class "label" ] [ Html.text l ] ]
        , Html.div [ class "field-body" ] [ Html.div [ class "field" ] [ Html.p [ class "control" ] [ control ] ] ]
        ]
