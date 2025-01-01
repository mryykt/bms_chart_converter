module Bms.Converter.Options.Edit exposing (..)

import Bms.Converter.Clustering.KernelFunction as Kernel exposing (KernelFunction)
import Bms.Converter.Options exposing (IncreaseScratchOptions, Options)
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
        [ text "number" "band width" bandWidth form
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
text typ l lens v =
    field l <|
        Html.input
            [ class "input", Attributes.type_ typ, onInput (Update << (Lens.compose value lens).setter), Attributes.value (.value <| lens.getter v) ]
            []


bool : String -> Lens a (Field Bool) -> a -> Html (Msg a)
bool l lens v =
    let
        lens_ =
            Lens.compose value lens
    in
    checkbox_ l (Lens.get lens_ v) (Update << lens_.setter)


checkbox_ : String -> Bool -> (Bool -> msg) -> Html msg
checkbox_ label checked msg =
    field label <| Html.label [ class "checkbox" ] [ Html.input [ Attributes.type_ "checkbox", onCheck msg, Attributes.checked checked ] [] ]


select : String -> Dict String b -> Lens a (Field b) -> a -> Html (Msg a)
select l dict { getter, setter } v =
    let
        option val =
            Html.option [ Attributes.value val, Attributes.selected (Dict.get val dict |> Maybe.unwrap False ((==) (getter v |> .value))) ] [ Html.text val ]

        get k =
            Dict.get k dict |> Maybe.withDefault (getter v |> .value)
    in
    field l <|
        Html.div [ class "select" ]
            [ Html.select
                [ onInput (Update << (Lens.compose value { getter = getter, setter = setter }).setter << get)
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
