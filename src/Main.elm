port module Main exposing (..)

import Bms.Converter exposing (convert)
import Bms.Converter.Options as Options exposing (Options)
import Bms.Converter.Options.Edit as OptionsEdit
import Bms.Load as Load
import Bms.Preview as Preview
import Bms.Save exposing (save)
import Bms.Types exposing (Bms, RawBms, decodeRawBms, defRawBms)
import Browser
import Bulma.Styled.CDN as CDN
import Bulma.Styled.Elements as Bulma exposing (buttonModifiers)
import Bulma.Styled.Modifiers exposing (loading, primary)
import Bulma.Styled.Modifiers.Typography exposing (Color(..))
import Css exposing (..)
import File exposing (File)
import File.Select as Select
import Html.Styled as Html exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Lazy exposing (lazy)
import Json.Decode exposing (Error, decodeValue)
import Json.Encode exposing (Value)
import Task


port compileBMS : { name : String, buf : String } -> Cmd msg


port loadBMS : (Value -> msg) -> Sub msg


type alias Model =
    { bms : Bms, options : Options, converted : Maybe Bms, state : State }


type alias State =
    { isShowOptions : Bool, isConverting : Bool }


defState : State
defState =
    { isShowOptions = False, isConverting = False }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { bms = Load.fromRawData defRawBms, options = Options.defOptions, converted = Nothing, state = defState }
    , Cmd.none
    )


type Msg
    = FileRequested
    | FileSelected File
    | FileLoaded String String
    | LoadBMS (Result Error RawBms)
    | EditOptions (OptionsEdit.Msg Options)
    | StartConverting
    | CompleteConverting Bms
    | SaveBms
    | UpdateState (State -> State)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ state } as model) =
    case msg of
        FileRequested ->
            ( model, Select.file [ ".bms", ".bme", ".bml", ".pms" ] FileSelected )

        FileSelected file ->
            ( model, Task.perform (FileLoaded <| File.name file) (File.toString file) )

        FileLoaded name buf ->
            ( model, compileBMS { name = name, buf = buf } )

        LoadBMS result ->
            case result of
                Ok raw ->
                    let
                        data =
                            Load.fromRawData raw
                    in
                    ( { model | bms = data }, Cmd.none )

                -- ( Test data (List.map Load.separateByMeasure <| groupingNotes data.header.waves <| Load.separeteLn data.notes), Cmd.none )
                Err _ ->
                    ( model, Cmd.none )

        EditOptions msg_ ->
            ( { model | options = OptionsEdit.update msg_ model.options }, Cmd.none )

        StartConverting ->
            ( { model | state = { state | isConverting = True } }, Task.perform (convert model.options >> CompleteConverting) (Task.succeed model.bms) )

        CompleteConverting converted ->
            ( { model | converted = Just converted, state = { state | isConverting = False } }, Cmd.none )

        SaveBms ->
            ( model
            , case model.converted of
                Just x ->
                    save x

                Nothing ->
                    Cmd.none
            )

        UpdateState setter ->
            ( { model | state = setter state }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    loadBMS (LoadBMS << decodeValue decodeRawBms)


view : Model -> Html Msg
view model =
    let
        button =
            Bulma.button { buttonModifiers | color = primary }
    in
    div [ css [ overflow auto, padding (px 10), margin (px 20) ] ]
        [ CDN.stylesheet
        , button [ onClick FileRequested ] [ text "file" ]
        , lazy Preview.view model.bms
        , case model.converted of
            Just bms ->
                lazy Preview.view bms

            Nothing ->
                div [] []
        , Bulma.button
            { buttonModifiers
                | state =
                    if model.state.isConverting then
                        loading

                    else
                        buttonModifiers.state
            }
            [ onClick StartConverting ]
            [ text "convert" ]
        , if model.converted /= Nothing then
            button [ onClick SaveBms ] [ text "save" ]

          else
            div [] []
        , if model.state.isShowOptions then
            div
                [ css
                    [ position fixed
                    , zIndex (int 200)
                    , width (pct 70)
                    , height (pct 100)
                    , top zero
                    , right zero
                    , backgroundColor (rgba 0 0 0 0.7)
                    ]
                ]
                [ OptionsEdit.view model.options |> Html.map EditOptions
                ]

          else
            div
                [ css
                    [ position fixed
                    , zIndex (int 200)
                    , width (px 50)
                    , height (pct 100)
                    , top zero
                    , right zero
                    , backgroundColor (rgba 0 0 0 0.7)
                    , hover [ cursor pointer ]
                    ]
                , onClick (UpdateState <| \x -> { x | isShowOptions = True })
                ]
                []
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = Html.toUnstyled << view
        , update = update
        , subscriptions = subscriptions
        }
