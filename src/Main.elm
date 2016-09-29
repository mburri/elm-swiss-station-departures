module Main exposing (main)

import Autocomplete
import Debug
import Html exposing (Html, button, div, text, h1, table, colgroup, col, thead, tbody, th, tr, td, input, p)
import Html.App as App
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (align, attribute, id, placeholder)
import Http
import Json.Decode as Json exposing ((:=))
import Platform.Cmd as Cmd
import Task


main : Program Never
main =
    App.program
        { init = init ! []
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Station =
    { name : String
    }


type alias Departure =
    { time : String
    , name : String
    , destination : String
    }


type alias Model =
    { query : String
    , stations : List Station
    , departures : List Departure
    , autoState : Autocomplete.State
    }


init : Model
init =
    { query = ""
    , stations = []
    , departures = initDepartures
    , autoState = Autocomplete.empty
    }


initDepartures : List Departure
initDepartures =
    [ { time = "14:22", name = "NFB28", destination = "Bern Wankdorf, Bahnhof" }
    , { time = "14:24", name = "NFB10", destination = "Ostermundigen, Rüti" }
    , { time = "14:25", name = "NFB10", destination = "Schliern bei Köniz" }
    ]



-- Update


type Msg
    = NoOp
    | ChangeQuery String
    | SearchStation
    | SearchStationFail Http.Error
    | SearchStationSucceed (List Station)


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
        , viewStations model.stations
        , viewAllDepartures model.departures
        ]


viewStations : List Station -> Html.Html Msg
viewStations stations =
    div [] (List.map viewStation stations)


viewStation : Station -> Html.Html Msg
viewStation station =
    p [] [ text station.name ]


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
        Task.perform SearchStationFail SearchStationSucceed (Http.get decodeStations url)


decodeStation : Json.Decoder Station
decodeStation =
    Json.object1 Station ("name" := Json.string)


decodeStations : Json.Decoder (List Station)
decodeStations =
    Json.object1 identity
        ("stations" := Json.list decodeStation)
