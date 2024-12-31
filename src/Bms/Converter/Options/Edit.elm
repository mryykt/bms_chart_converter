module Bms.Converter.Options.Edit exposing (..)

import Bms.Converter.Clustering.KernelFunction as Kernel exposing (KernelFunction)
import Bms.Converter.Options exposing (IncreaseScratchOptions, Optional, Options)
import Bms.Converter.Options.Lens as Lens exposing (..)
import Dict exposing (Dict)
import Form.Decoder as D exposing (Decoder)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (class)
import Html.Styled.Events exposing (onCheck, onInput)
import List.Extra as List
import Maybe.Extra as Maybe


type Msg a
    = Default
    | Update (a -> a)


type alias Field a =
    { value : a, invalid : Bool }


type alias Form =
    { bandWidth : Field String
    , kernelFunction : Field KernelFunction

    --
    , increaseScratchOptions : Field Bool
    , minDuration : Field String
    , isIncludeLn : Field Bool
    }


initField : a -> Field a
initField x =
    Field x False


initForm : Form
initForm =
    { bandWidth = initField "480"
    , kernelFunction = initField Kernel.Gauss
    , increaseScratchOptions = initField False
    , minDuration = initField "8"
    , isIncludeLn = initField True
    }


fromForm : Form -> ( Form, Maybe Options )
fromForm form =
    case D.run decoder form of
        Ok options ->
            ( form, Just options )

        Err errs ->
            ( List.foldl (\errField form_ -> Lens.update errField (\x -> { x | invalid = True }) form_) form errs, Nothing )


decoder : Decoder Form (Lens Form (Field String)) Options
decoder =
    let
        increaseScratchOptions : Decoder Form (Lens Form (Field String)) (Maybe IncreaseScratchOptions)
        increaseScratchOptions =
            D.custom <|
                \form ->
                    if form.increaseScratchOptions.value then
                        D.run
                            (D.top IncreaseScratchOptions
                                |> intField minDuration
                                |> identityField isIncludeLn
                                |> D.map Just
                            )
                            form

                    else
                        Ok Nothing
    in
    D.top Options |> floatField bandWidth |> identityField kernelFunction |> D.field increaseScratchOptions


intField : Lens a (Field String) -> Decoder a (Lens a (Field String)) (Int -> b) -> Decoder a (Lens a (Field String)) b
intField lens =
    D.field (D.lift (.value << get lens) (D.int lens))


floatField : Lens a (Field String) -> Decoder a (Lens a (Field String)) (Float -> b) -> Decoder a (Lens a (Field String)) b
floatField lens =
    D.field (D.lift (.value << get lens) (D.float lens))


identityField : Lens a (Field b) -> Decoder a c (b -> d) -> Decoder a c d
identityField lens =
    D.field (D.lift (.value << get lens) D.identity)


update : Msg Form -> Form -> Form
update msg form =
    case msg of
        Default ->
            initForm

        Update setter ->
            setter form


view : Form -> Html (Msg Form)
view form =
    Html.div []
        []


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
