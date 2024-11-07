port module Main exposing (..)

import BMS.Convert as Conv
import BMS.Types exposing (BMS, Note, RawBMS, decodeRawBMS)
import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
            ( model, Select.file [ "text" ] FileSelected )

        ( Init _, FileSelected file ) ->
            ( Init (Just <| File.name file), Task.perform FileLoaded (File.toString file) )

        ( Init (Just name), FileLoaded buf ) ->
            ( model, compileBMS { name = name, buf = buf } )

        ( Init (Just _), LoadBMS result ) ->
            case result of
                Ok raw ->
                    let
                        data =
                            Conv.fromRawData raw
                    in
                    ( Preview data (Conv.separateByMeasure data.notes), Cmd.none )

                Err _ ->
                    Debug.todo ""

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    loadBMS (LoadBMS << decodeValue decodeRawBMS)


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "hello" ]
        , button [ onClick FileRequested ] [ text "file" ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
