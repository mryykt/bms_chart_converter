port module Experimental exposing (..)

import BTime
import Bms.Converter exposing (groupingNotes)
import Bms.Load as Load
import Bms.Preview as Preview
import Bms.Types exposing (Bms, Note, RawBms, decodeRawBms, sort)
import Browser
import Chart as C
import Chart.Attributes as CA
import Clustering
import Clustering.KernelFunction as KernelFunction
import Css exposing (..)
import Html as UH
import Html.Styled as H exposing (Html, div)
import Html.Styled.Lazy exposing (lazy)
import Json.Decode exposing (Error, decodeValue)
import Json.Encode exposing (Value)
import List.Extra as List
import List.Nonempty as Nonempty
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
                        |> List.concatMap (Clustering.rough bms.mlens)
                        |> List.concatMap (Clustering.clustering 0.5 KernelFunction.gauss BTime.toFloat)

                groupedNotes =
                    List.indexedMap (\i notes -> List.map (\note -> { note | value = i }) <| Nonempty.toList notes) group |> List.concat |> sort
            in
            div [ css [ position relative, width (px 900), padding (px 50) ] ]
                [ lazy Preview.groupedView { bms | notes = groupedNotes } ]


testView : List Note -> UH.Html msg
testView notes =
    let
        times =
            List.map BTime.toFloat notes

        mint =
            Maybe.withDefault 0 <| List.head times

        maxt =
            Maybe.withDefault 0 <| List.last times

        density =
            Clustering.density KernelFunction.gauss 0.3 times

        data1 =
            List.map (\t -> { t = t, y = density t }) <| rangef mint maxt 0.1

        data2 =
            List.map (\t -> { t = t, y = 0.1 }) times
    in
    C.chart [ CA.width 900, CA.height 300, CA.padding { top = 30, bottom = 10, left = 10, right = 10 } ]
        [ C.xLabels []
        , C.yLabels []
        , C.series .t
            [ C.interpolated .y [ CA.monotone ] [ CA.circle ]
            ]
            data1
        , C.series .t
            [ C.scatter .y [] ]
            data2
        ]


rangef : Float -> Float -> Float -> List Float
rangef minv maxv step =
    if minv < maxv then
        minv :: rangef (minv + step) maxv step

    else
        []


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = H.toUnstyled << view
        , update = update
        , subscriptions = subscriptions
        }
