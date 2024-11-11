port module Main exposing (..)

import Bms.Load as Load
import Bms.Preview as Preview
import Bms.Types exposing (BMS, Note, RawBMS, decodeRawBMS)
import Browser
import File exposing (File)
import File.Select as Select
import Html.Styled as Html exposing (Html, button, div, text)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Lazy exposing (lazy2)
import Json.Decode exposing (Error, decodeValue)
import Json.Encode exposing (Value)
import Task


port compileBMS : { name : String, buf : String } -> Cmd msg


port loadBMS : (Value -> msg) -> Sub msg


type Model
    = Init (Maybe String)
    | Preview BMS (List ( Int, List Note ))


init : () -> ( Model, Cmd Msg )
init _ =
    ( Init Nothing
    , Cmd.none
    )


type Msg
    = FileRequested
    | FileSelected File
    | FileLoaded String
    | LoadBMS (Result Error RawBMS)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Init _, FileRequested ) ->
            ( model, Select.file [ ".bms", ".bme", ".bml", ".pms" ] FileSelected )

        ( Init _, FileSelected file ) ->
            ( Init (Just <| File.name file), Task.perform FileLoaded (File.toString file) )

        ( Init (Just name), FileLoaded buf ) ->
            ( model, compileBMS { name = name, buf = buf } )

        ( Init (Just _), LoadBMS result ) ->
            case result of
                Ok raw ->
                    let
                        data =
                            Load.fromRawData raw
                    in
                    ( Preview data (Load.separateByMeasure <| Load.separeteLn data.notes), Cmd.none )

                Err _ ->
                    Debug.todo ""

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    loadBMS (LoadBMS << decodeValue decodeRawBMS)


view : Model -> Html Msg
view model =
    case model of
        Init _ ->
            div []
                [ button [ onClick FileRequested ] [ text "file" ]
                ]

        Preview bms sep ->
            lazy2 Preview.view bms sep


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = Html.toUnstyled << view
        , update = update
        , subscriptions = subscriptions
        }
