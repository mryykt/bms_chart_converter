port module Experimental exposing (..)

import Bms.Converter exposing (convert, groupingNotes)
import Bms.Converter.Options exposing (Options, defIncreaseScratchOptions, defOptions)
import Bms.Load as Load
import Bms.Preview as Preview
import Bms.Types exposing (Bms, RawBms, decodeRawBms, sort)
import Browser
import Clustering
import Css exposing (..)
import Html.Styled as H exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Lazy exposing (lazy)
import Json.Decode exposing (Error, decodeValue)
import Json.Encode exposing (Value)
import List.Extra as List
import List.Nonempty as Nonempty
import Maybe.Extra as Maybe
import SampleData exposing (..)
import Task


port compileBMS : { name : String, buf : String } -> Cmd msg


port loadBMS : (Value -> msg) -> Sub msg


type Model
    = Init
    | Model Options Bms (Maybe Bms)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Init
    , compileBMS { name = sampleData.name, buf = sampleData.body }
    )


type Msg
    = LoadBMS (Result Error RawBms)
    | CompleteConvert Bms


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Init, LoadBMS result ) ->
            case result of
                Ok raw ->
                    let
                        data =
                            Load.fromRawData raw

                        options =
                            { defOptions | inscreaseScratchOptions = Just defIncreaseScratchOptions }
                    in
                    ( Model options data Nothing
                    , Task.perform
                        (convert data.chartType options >> CompleteConvert)
                        (Task.succeed data)
                    )

                Err _ ->
                    Debug.todo ""

        ( Model options bms Nothing, CompleteConvert converted ) ->
            ( Model options bms (Just converted), Cmd.none )

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

        Model options bms converted ->
            let
                group =
                    groupingNotes bms.header.waves bms.notes
                        |> List.concatMap Clustering.rough
                        |> List.concatMap (Clustering.clustering options.bandWidth options.kernelFunction .time)

                groupedNotes =
                    List.indexedMap (\i notes -> List.map (\note -> { note | value = i }) <| Nonempty.toList notes) group |> List.concat |> sort
            in
            div [ css [ position relative, width (px 900), padding (px 50) ] ]
                [ lazy Preview.groupedView { bms | notes = groupedNotes }
                , Maybe.unwrap (div [] []) (lazy Preview.view) converted
                ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = H.toUnstyled << view
        , update = update
        , subscriptions = subscriptions
        }
