module Main exposing (main)

import Html exposing (Html, button, div, text, h1, table, colgroup, col, thead, tbody, th, tr, td, input)
import Html.App as App
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (align, attribute, id, placeholder)
import Platform.Cmd as Cmd
import Task
import Http
import Json.Decode as Json


main : Program Never
main =
    App.program
        { init = init "" ""
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { query : String
    , stations : String
    }


init : String -> String -> ( Model, Cmd Msg )
init query stations =
    ( Model query stations
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | ChangeQuery String
    | SearchStation
    | SearchStationFail Http.Error
    | SearchStationSucceed String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ChangeQuery query ->
            ( { model | query = query }, Cmd.none )

        SearchStation ->
            ( model, getStations model.query )

        SearchStationFail error ->
            ( model, Cmd.none )

        SearchStationSucceed stations ->
            ( { model | stations = stations }, Cmd.none )



-- View


view : Model -> Html.Html Msg
view model =
    div []
        [ h1 [] [ text "Swiss Station Departures" ]
        , div []
            [ input [ placeholder "Station", onInput ChangeQuery ]
                []
            , button [ onClick SearchStation ] [ text "Search" ]
            ]
        , div []
            [ text model.query
            , text " "
            , text model.stations
            ]
        , table [ id "stationboard" ]
            [ colgroup []
                [ col [ attribute "width" "120" ]
                    []
                , col [ attribute "width" "140" ]
                    []
                , col [ attribute "width" "230" ]
                    []
                ]
            , thead []
                [ tr []
                    [ th [ align "left" ]
                        [ text "Zeit" ]
                    , th []
                        [ text "" ]
                    , th [ align "left" ]
                        [ text "Nach" ]
                    ]
                ]
            , tbody []
                [ tr []
                    [ td []
                        [ text "14:22" ]
                    , td []
                        [ text "NFB 28" ]
                    , td []
                        [ text "Bern Wankdorf, Bahnhof" ]
                    ]
                , tr []
                    [ td []
                        [ text "14:24" ]
                    , td []
                        [ text "NFB 10" ]
                    , td []
                        [ text "Schliern bei Köniz" ]
                    ]
                , tr []
                    [ td []
                        [ text "14:25" ]
                    , td []
                        [ text "NFB 10" ]
                    , td []
                        [ text "Ostermundigen, Rüti" ]
                    ]
                , tr []
                    [ td []
                        [ text "14:29" ]
                    , td []
                        [ text "NFB 10" ]
                    , td []
                        [ text "Schliern bei Köniz" ]
                    ]
                , tr []
                    [ td []
                        [ text "14:30" ]
                    , td []
                        [ text "NFB 10" ]
                    , td []
                        [ text "Ostermundigen, Rüti" ]
                    ]
                , tr []
                    [ td []
                        [ text "14:34" ]
                    , td []
                        [ text "NFB 10" ]
                    , td []
                        [ text "Schliern bei Köniz" ]
                    ]
                , tr []
                    [ td []
                        [ text "14:35" ]
                    , td []
                        [ text "NFB 10" ]
                    , td []
                        [ text "Ostermundigen, Rüti" ]
                    ]
                , tr []
                    [ td []
                        [ text "14:35" ]
                    , td []
                        [ text "10" ]
                    , td []
                        [ text "Ostermundigen" ]
                    ]
                , tr []
                    [ td []
                        [ text "14:37" ]
                    , td []
                        [ text "NFB 28" ]
                    , td []
                        [ text "Bern Wankdorf, Bahnhof" ]
                    ]
                , tr []
                    [ td []
                        [ text "14:39" ]
                    , td []
                        [ text "NFB 10" ]
                    , td []
                        [ text "Schliern bei Köniz" ]
                    ]
                , tr []
                    [ td []
                        [ text "14:40" ]
                    , td []
                        [ text "NFB 10" ]
                    , td []
                        [ text "Ostermundigen, Rüti" ]
                    ]
                , tr []
                    [ td []
                        [ text "14:44" ]
                    , td []
                        [ text "NFB 10" ]
                    , td []
                        [ text "Schliern bei Köniz" ]
                    ]
                , tr []
                    [ td []
                        [ text "14:45" ]
                    , td []
                        [ text "NFB 10" ]
                    , td []
                        [ text "Ostermundigen, Rüti" ]
                    ]
                , tr []
                    [ td []
                        [ text "14:49" ]
                    , td []
                        [ text "NFB 10" ]
                    , td []
                        [ text "Schliern bei Köniz" ]
                    ]
                , tr []
                    [ td []
                        [ text "14:50" ]
                    , td []
                        [ text "NFB 10" ]
                    , td []
                        [ text "Ostermundigen, Rüti" ]
                    ]
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getStations : String -> Cmd Msg
getStations query =
    let
        url =
            "http://transport.opendata.ch/v1/locations?query=" ++ query
    in
        Task.perform SearchStationFail SearchStationSucceed (Http.get decodeStation url)


decodeStation : Json.Decoder String
decodeStation =
    Json.at [ "stations", "name" ] Json.string
