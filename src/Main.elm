port module Main exposing (..)

import Browser
import Bytes exposing (Bytes)
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task


port loadBMS : { name : String, buf : String } -> Cmd msg


type Model
    = Init (Maybe String)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Init Nothing
    , Cmd.none
    )


type Msg
    = FileRequested
    | FileSelected File
    | FileLoaded String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Init _, FileRequested ) ->
            ( model, Select.file [ "text/plain" ] FileSelected )

        ( Init _, FileSelected file ) ->
            ( Init (Just <| File.name file), Task.perform FileLoaded (File.toString file) )

        ( Init (Just name), FileLoaded buf ) ->
            ( model, loadBMS { name = name, buf = buf } )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


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
