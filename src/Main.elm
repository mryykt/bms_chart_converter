port module Main exposing (..)

import Basics.Extra2 exposing (ifelse, just)
import Bms.Converter exposing (convert)
import Bms.Converter.Options exposing (Options)
import Bms.Converter.Options.Edit as OptionsEdit
import Bms.Converter.Options.Form as Form exposing (Form)
import Bms.Load as Load
import Bms.Preview as Preview
import Bms.Save exposing (save)
import Bms.Types exposing (Bms, RawBms, decodeRawBms, defRawBms)
import Browser
import Css exposing (..)
import File exposing (File)
import File.Select as Select
import Html.Styled as Html exposing (Html, div, text)
import Html.Styled.Attributes as Attributes exposing (class, css)
import Html.Styled.Bulma as Bulma
import Html.Styled.Events exposing (onClick)
import Html.Styled.Extra exposing (whenHtml, whenJustHtml)
import Html.Styled.Lazy exposing (lazy)
import Json.Decode exposing (Error, decodeValue)
import Json.Encode exposing (Value)
import Process
import Task


port compileBMS : { name : String, buf : String } -> Cmd msg


port loadBMS : (Value -> msg) -> Sub msg


type alias Model =
    { bms : Bms, options : Maybe Options, converted : Maybe Bms, state : State }


type alias State =
    { tab : Tab, optionsForm : Form, isConverting : Bool }


defState : State
defState =
    { tab = Original, optionsForm = Form.init, isConverting = False }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { bms = Load.fromRawData defRawBms, options = Form.from defState.optionsForm |> Tuple.second, converted = Nothing, state = defState }
    , Cmd.none
    )


type Msg
    = FileRequested
    | FileSelected File
    | FileLoaded String String
    | LoadBMS (Result Error RawBms)
    | EditOptions (OptionsEdit.Msg Form)
    | StartConverting
    | CompleteConverting Bms
    | SaveBms
    | UpdateState (State -> State)
    | TabSelected Tab


type Tab
    = Original
    | Option
    | Converted
    | Diff


tabToString : Tab -> String
tabToString tab =
    case tab of
        Original ->
            "Original"

        Option ->
            "Option"

        Converted ->
            "Converted"

        Diff ->
            "Diff"


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

                        ( newForm, newOptions ) =
                            Form.setChartType data.chartType model.state.optionsForm |> Form.from
                    in
                    ( { model
                        | bms = data
                        , converted = Nothing
                        , options = newOptions
                        , state = { state | optionsForm = newForm }
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        EditOptions msg_ ->
            let
                ( newForm, newOptions ) =
                    OptionsEdit.update msg_ model.state.optionsForm |> Form.from
            in
            ( { model | options = newOptions, state = { state | optionsForm = newForm } }, Cmd.none )

        StartConverting ->
            ( { model | state = { state | isConverting = True } }
            , just model.options
                (\options -> Task.perform <| CompleteConverting << convert options)
                (always Cmd.none)
                (Process.sleep 100 |> Task.andThen (\_ -> Task.succeed model.bms))
            )

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

        TabSelected tab ->
            ( { model | state = { state | tab = tab } }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    loadBMS (LoadBMS << decodeValue decodeRawBms)


view : Model -> Html Msg
view model =
    div [ css [ overflow auto, padding (px 10) ] ]
        [ div [ class "buttons" ]
            [ Bulma.file "Choose a file..." FileRequested (Just model.bms.name)
            , Html.button
                [ class "button"
                , class <| ifelse model.state.isConverting "is-loading" "is-info"
                , Attributes.disabled (model.options == Nothing)
                , css [ marginBottom (px 24) ]
                , onClick StartConverting
                ]
                [ text "convert" ]
            , whenHtml (not model.state.isConverting && model.converted /= Nothing) <|
                Html.button [ class "button is-primary", css [ marginBottom (px 24) ], onClick SaveBms ] [ text "save" ]
            ]
        , let
            tabs =
                [ Original, Option ]
                    ++ just model.converted
                        (\converted ->
                            Converted :: ifelse (model.bms.chartType == converted.chartType) [ Diff ] []
                        )
                        []
          in
          Bulma.tabs tabToString tabs TabSelected model.state.tab
        , case model.state.tab of
            Original ->
                lazy Preview.view model.bms

            Option ->
                Html.map EditOptions <| OptionsEdit.view model.state.optionsForm

            Converted ->
                whenJustHtml model.converted (lazy Preview.view)

            Diff ->
                whenJustHtml model.converted (lazy (Preview.diffView model.bms))
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = Html.toUnstyled << view
        , update = update
        , subscriptions = subscriptions
        }
