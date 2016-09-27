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
        { init = init "" "" initDepartures
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Departure =
    { time : String
    , name : String
    , destination : String
    }


initDepartures : List Departure
initDepartures =
    [ { time = "14:22", name = "NFB28", destination = "Bern Wankdorf, Bahnhof" }
    , { time = "14:24", name = "NFB10", destination = "Ostermundigen, Rüti" }
    , { time = "14:25", name = "NFB10", destination = "Schliern bei Köniz" }
    ]


type alias Model =
    { query : String
    , stations : String
    , departures : List Departure
    }


init : String -> String -> List Departure -> ( Model, Cmd Msg )
init query stations departures =
    ( Model query stations departures
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
        , viewAllDepartures model.departures
        ]


viewAllDepartures : List Departure -> Html.Html Msg
viewAllDepartures departures =
    table [ id "stationboard" ]
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
        , tbody [] (List.map viewSingleDeparture departures)
        ]


viewSingleDeparture : Departure -> Html.Html Msg
viewSingleDeparture departure =
    tr []
        [ td []
            [ text departure.time ]
        , td []
            [ text departure.name ]
        , td []
            [ text departure.destination ]
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
