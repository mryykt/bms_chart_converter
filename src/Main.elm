port module Main exposing (..)

import Bms.Converter.Options as Options exposing (Options)
import Bms.Converter.Options.Edit as OptionsEdit
import Bms.Load as Load
import Bms.Preview as Preview
import Bms.Types exposing (Bms, RawBms, decodeRawBms)
import Browser
import Css exposing (overflow, padding, px, scroll)
import File exposing (File)
import File.Select as Select
import Html.Styled as Html exposing (Html, button, div, text)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Lazy exposing (lazy)
import Json.Decode exposing (Error, decodeValue)
import Json.Encode exposing (Value)
import Task


port compileBMS : { name : String, buf : String } -> Cmd msg


port loadBMS : (Value -> msg) -> Sub msg


type Model
    = Init (Maybe String)
    | Model Bms Options


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
                    ( Model data Options.defOptions, Cmd.none )

                -- ( Test data (List.map Load.separateByMeasure <| groupingNotes data.header.waves <| Load.separeteLn data.notes), Cmd.none )
                Err _ ->
                    ( model, Cmd.none )

        EditOptions msg_ ->
            case model of
                Model bms options ->
                    ( Model bms (OptionsEdit.update msg_ options), Cmd.none )

                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    loadBMS (LoadBMS << decodeValue decodeRawBms)


view : Model -> Html Msg
view model =
    div [ css [ overflow scroll, padding (px 10) ] ] <|
        button [ onClick FileRequested ] [ text "file" ]
            :: (case model of
                    Model bms options ->
                        [ lazy Preview.view bms
                        , OptionsEdit.view options |> Html.map EditOptions
                        , button [] [ text "convert" ]
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
