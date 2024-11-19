port module Experimental exposing (..)

import BTime
import Bms.Converter exposing (groupingNotes)
import Bms.Load as Load
import Bms.Types exposing (Bms, RawBms, decodeRawBms)
import Browser
import Clustering
import Css exposing (..)
import Html.Styled as Html exposing (Html, div)
import Json.Decode exposing (Error, decodeValue)
import Json.Encode exposing (Value)
import List.Extra as List
import Maybe.Extra as Maybe
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

                f notes =
                    let
                        mint =
                            Maybe.unwrap 0 BTime.toFloat <| List.head notes

                        maxt =
                            Maybe.unwrap 0 BTime.toFloat <| List.last notes

                        density =
                            Clustering.density gauss 0.4 <| List.map BTime.toFloat notes

                        gauss x =
                            1 / sqrt (2 * pi) * e ^ (-x * x / 2)
                    in
                    Clustering.testView mint maxt density
            in
            div [ css [ position relative, width (px 300), padding (px 50) ] ] <| List.map (f >> Html.fromUnstyled) group


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = Html.toUnstyled << view
        , update = update
        , subscriptions = subscriptions
        }
