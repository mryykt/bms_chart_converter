port module Main exposing (..)

import Bms.Converter exposing (convert)
import Bms.Converter.Options as Options exposing (Options)
import Bms.Converter.Options.Edit as OptionsEdit
import Bms.Load as Load
import Bms.Preview as Preview
import Bms.Save exposing (save)
import Bms.Types exposing (Bms, RawBms, decodeRawBms)
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


type Model
    = Init (Maybe String)
    | Model { bms : Bms, options : Options, converted : Maybe Bms, state : State }


type alias State =
    { isShowOptions : Bool, isConverting : Bool }


defState : State
defState =
    { isShowOptions = False, isConverting = False }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Init Nothing
    , Cmd.none
    )


type Msg
    = FileRequested
    | FileSelected File
    | FileLoaded String
    | LoadBMS (Result Error RawBms)
    | EditOptions (OptionsEdit.Msg Options)
    | StartConverting
    | CompleteConverting Bms
    | SaveBms
    | UpdateState (State -> State)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FileRequested ->
            ( model, Select.file [ ".bms", ".bme", ".bml", ".pms" ] FileSelected )

        FileSelected file ->
            ( Init <| Just <| File.name file, Task.perform FileLoaded (File.toString file) )

        FileLoaded buf ->
            case model of
                Init (Just name) ->
                    ( model, compileBMS { name = name, buf = buf } )

                _ ->
                    ( model, Cmd.none )

        LoadBMS result ->
            case result of
                Ok raw ->
                    let
                        data =
                            Load.fromRawData raw
                    in
                    ( Model { bms = data, options = Options.defOptions, converted = Nothing, state = defState }, Cmd.none )

                -- ( Test data (List.map Load.separateByMeasure <| groupingNotes data.header.waves <| Load.separeteLn data.notes), Cmd.none )
                Err _ ->
                    ( model, Cmd.none )

        EditOptions msg_ ->
            case model of
                Model model_ ->
                    ( Model { model_ | options = OptionsEdit.update msg_ model_.options }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StartConverting ->
            case model of
                Model ({ state } as model_) ->
                    ( Model { model_ | state = { state | isConverting = True } }, Task.perform (convert model_.options >> CompleteConverting) (Task.succeed model_.bms) )

                _ ->
                    ( model, Cmd.none )

        CompleteConverting converted ->
            case model of
                Model ({ state } as model_) ->
                    ( Model { model_ | converted = Just converted, state = { state | isConverting = False } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SaveBms ->
            case model of
                Model { converted } ->
                    ( model
                    , case converted of
                        Just x ->
                            save x

                        Nothing ->
                            Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateState setter ->
            case model of
                Model ({ state } as model_) ->
                    ( Model { model_ | state = setter state }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    loadBMS (LoadBMS << decodeValue decodeRawBms)


view : Model -> Html Msg
view model =
    let
        button =
            Bulma.button { buttonModifiers | color = primary }
    in
    div [ css [ overflow auto, padding (px 10), margin (px 20) ] ] <|
        [ CDN.stylesheet, button [ onClick FileRequested ] [ text "file" ] ]
            ++ (case model of
                    Model model_ ->
                        [ lazy Preview.view model_.bms
                        , case model_.converted of
                            Just bms_ ->
                                lazy Preview.view bms_

                            Nothing ->
                                div [] []
                        , Bulma.button
                            { buttonModifiers
                                | state =
                                    if model_.state.isConverting then
                                        loading

                                    else
                                        buttonModifiers.state
                            }
                            [ onClick StartConverting ]
                            [ text "convert" ]
                        , if model_.converted /= Nothing then
                            button [ onClick SaveBms ] [ text "save" ]

                          else
                            div [] []
                        , if model_.state.isShowOptions then
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
                                [ OptionsEdit.view model_.options |> Html.map EditOptions
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

                    _ ->
                        []
               )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = Html.toUnstyled << view
        , update = update
        , subscriptions = subscriptions
        }
