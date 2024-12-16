module Bms.Converter.Options.Edit exposing (..)

import Bms.Converter.Clustering.KernelFunction as Kernel
import Bms.Converter.Options exposing (Options, defOptions)
import Bulma.Styled.Form as Form exposing (Control, Field, controlSelect, controlSelectModifiers)
import Bulma.Styled.Modifiers exposing (standard)
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events exposing (onInput)
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
        [ float "test" { getter = .bandWidth, setter = \x y -> { y | bandWidth = x } } options
        , select "kernel function"
            (Dict.fromList
                [ ( "Gauss", Kernel.gauss )
                , ( "Linear", Kernel.linear )
                , ( "Exp", Kernel.exp )
                , ( "Tophat", Kernel.tophat )
                , ( "Epanechnikov", Kernel.epanechnikov )
                ]
            )
            { getter = .kernelFunction, setter = \x y -> { y | kernelFunction = x } }
            options
        ]


text : String -> Lens a String -> a -> Field (Msg a)
text l { getter, setter } v =
    field l <|
        Form.controlInput Form.controlInputModifiers
            []
            [ onInput (Update << setter), Attributes.value (getter v) ]
            []


int : String -> Lens a Int -> a -> Field (Msg a)
int l { getter, setter } =
    text l
        { getter = getter >> String.fromInt
        , setter = setter << Maybe.withDefault 0 << String.toInt
        }


float : String -> Lens a Float -> a -> Field (Msg a)
float l { getter, setter } =
    text l
        { getter = getter >> String.fromFloat
        , setter = setter << Maybe.withDefault 0 << String.toFloat
        }


select : String -> Dict String b -> Lens a b -> a -> Field (Msg a)
select l dict { getter, setter } v =
    let
        option val =
            Html.option [ Attributes.value val ] [ Html.text val ]

        get k =
            Dict.get k dict |> Maybe.withDefault (getter v)
    in
    field l <|
        controlSelect controlSelectModifiers
            []
            [ onInput (Update << setter << get)
            ]
        <|
            List.map option <|
                Dict.keys dict


field : String -> Control msg -> Field msg
field l control =
    Form.horizontalFields []
        [ Form.fieldLabel standard
            []
            [ Form.label []
                [ Html.text l
                ]
            ]
        , Form.control Form.controlModifiers [] [ control ]
        ]
