port module Experimental exposing (..)

import Bms.Converter exposing (groupingNotes)
import Bms.Load as Load
import Bms.Preview as Preview
import Bms.Types exposing (Bms, RawBms, decodeRawBms, sort)
import Browser
import Clustering
import Clustering.KernelFunction as KernelFunction
import Css exposing (..)
import Html.Styled as H exposing (Html, div)
import Html.Styled.Lazy exposing (lazy)
import Json.Decode exposing (Error, decodeValue)
import Json.Encode exposing (Value)
import List.Extra as List
import List.Nonempty as Nonempty
import SampleData exposing (..)
import Svg.Styled.Attributes exposing (css)


port compileBMS : { name : String, buf : String } -> Cmd msg


port loadBMS : (Value -> msg) -> Sub msg


type Model
    = Init
    | Model Bms


init : () -> ( Model, Cmd Msg )
init _ =
    ( Init
    , compileBMS { name = sampleData.name, buf = sampleData.body }
    )


type Msg
    = LoadBMS (Result Error RawBms)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Init, LoadBMS result ) ->
            case result of
                Ok raw ->
                    let
                        data =
                            Load.fromRawData raw
                    in
                    ( Model data, Cmd.none )

                Err _ ->
                    Debug.todo ""

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    loadBMS (LoadBMS << decodeValue decodeRawBms)


view : Model -> Html Msg
view model =
    case model of
        Init ->
            div []
                []

        Model bms ->
            let
                group =
                    groupingNotes bms.header.waves bms.notes
                        |> List.concatMap Clustering.rough
                        |> List.concatMap (Clustering.clustering 480 KernelFunction.gauss .time)

                groupedNotes =
                    List.indexedMap (\i notes -> List.map (\note -> { note | value = i }) <| Nonempty.toList notes) group |> List.concat |> sort
            in
            div [ css [ position relative, width (px 900), padding (px 50) ] ]
                [ lazy Preview.groupedView { bms | notes = groupedNotes } ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = H.toUnstyled << view
        , update = update
        , subscriptions = subscriptions
        }
