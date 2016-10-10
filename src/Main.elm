module Main exposing (..)

import Autocomplete
import Date exposing (Date)
import Html.App as App
import Html exposing (Html, button, div, text, h1, input, td, th, tr, thead, tbody, table)
import Html.Attributes exposing (id, classList, class, value, autocomplete, style, attribute, align, placeholder)
import Html.Events exposing (onInput, onFocus, onWithOptions, keyCode)
import Http
import Json.Decode as Json exposing ((:=))
import String
import Task
import Debug


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Station =
    { name : String
    }


type alias Departure =
    { to : String
    , departure : String
    , name : String
    }


type alias Model =
    { query : String
    , autoState : Autocomplete.State
    , stations : List Station
    , howManyToShow : Int
    , showStations : Bool
    , selectedStation : Maybe Station
    , departures : List Departure
    , fetchStationTableFailedMessage : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        ""
        Autocomplete.empty
        []
        5
        False
        Nothing
        []
        ""
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | ChangeQuery String
    | SetAutoState Autocomplete.Msg
    | SelectStation String
    | Wrap Bool
    | Reset
    | FetchStationTableSucceed (List Departure)
    | FetchStationTableFail Http.Error
    | FetchStationSucceed (List Station)
    | FetchStationFail Http.Error
    | HandleEscape


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ChangeQuery q ->
            ( { model | query = q }, getStations q )

        SetAutoState autoMsg ->
            let
                ( newState, maybeMsg ) =
                    Autocomplete.update updateConfig autoMsg model.howManyToShow model.autoState (acceptableStations model.query model.stations)

                newModel =
                    { model | autoState = newState }
            in
                case maybeMsg of
                    Nothing ->
                        ( newModel, Cmd.none )

                    Just updateMsg ->
                        update updateMsg newModel

        SelectStation id ->
            let
                selectedStation =
                    List.head (List.filter (\station -> station.name == id) model.stations)
            in
                ( { model
                    | query =
                        List.filter (\station -> station.name == id) model.stations
                            |> List.head
                            |> Maybe.withDefault (Station "")
                            |> .name
                    , autoState = Autocomplete.empty
                    , showStations = False
                    , selectedStation = selectedStation
                  }
                , getStationTable selectedStation
                )

        Reset ->
            { model | autoState = Autocomplete.reset updateConfig model.autoState, selectedStation = Nothing } ! []

        Wrap toTop ->
            case model.selectedStation of
                Just station ->
                    update Reset model

                Nothing ->
                    if toTop then
                        { model
                            | autoState = Autocomplete.resetToLastItem updateConfig (acceptableStations model.query model.stations) model.howManyToShow model.autoState
                            , selectedStation = List.head <| List.reverse <| List.take model.howManyToShow <| (acceptableStations model.query model.stations)
                        }
                            ! []
                    else
                        { model
                            | autoState = Autocomplete.resetToFirstItem updateConfig (acceptableStations model.query model.stations) model.howManyToShow model.autoState
                            , selectedStation = List.head <| List.take model.howManyToShow <| (acceptableStations model.query model.stations)
                        }
                            ! []

        FetchStationTableSucceed result ->
            ( { model | departures = result }, Cmd.none )

        FetchStationTableFail error ->
            ( { model | fetchStationTableFailedMessage = toString error }, Cmd.none )

        FetchStationFail error ->
            ( model, Cmd.none )

        FetchStationSucceed stations ->
            ( { model
                | stations = stations
                , showStations = True
              }
            , Cmd.none
            )

        HandleEscape ->
            ( { model
                | query = ""
                , selectedStation = Nothing
                , departures = []
                , stations = []
                , autoState = Autocomplete.empty
              }
            , Cmd.none
            )


getStations : String -> Cmd Msg
getStations query =
    let
        url =
            "http://transport.opendata.ch/v1/locations?query=" ++ query
    in
        Task.perform FetchStationFail FetchStationSucceed (Http.get decodeStations url)


decodeStation : Json.Decoder Station
decodeStation =
    Json.object1 Station ("name" := Json.string)


decodeStations : Json.Decoder (List Station)
decodeStations =
    Json.object1 identity
        ("stations" := Json.list decodeStation)


getStationTable : Maybe Station -> Cmd Msg
getStationTable maybeStation =
    case maybeStation of
        Just station ->
            let
                url =
                    "http://transport.opendata.ch/v1/stationboard?station=" ++ station.name ++ "&limit=20"
            in
                Task.perform FetchStationTableFail FetchStationTableSucceed (Http.get decodeDepartures url)

        Nothing ->
            Cmd.none


decodeDepartures : Json.Decoder (List Departure)
decodeDepartures =
    Json.object1 identity ("stationboard" := Json.list decodeDeparture)


decodeDeparture : Json.Decoder Departure
decodeDeparture =
    Json.object3 Departure
        ("to" := Json.string)
        (Json.at [ "stop", "departure" ] Json.string)
        ("name" := Json.string)


acceptableStations : String -> List Station -> List Station
acceptableStations query stations =
    List.filter (matches query) stations


matches : String -> Station -> Bool
matches query station =
    String.contains (String.toLower query) (String.toLower station.name)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map SetAutoState Autocomplete.subscription



-- VIEW


view model =
    let
        options =
            { preventDefault = True, stopPropagation = False }

        dec =
            -- TODO: naming?
            (Json.customDecoder keyCode
                (\code ->
                    if code == 38 || code == 40 then
                        Ok NoOp
                    else if code == 27 then
                        Ok HandleEscape
                    else
                        Err "not handling that key"
                )
            )
    in
        div []
            [ h1 [] [ text "elm-swiss-station-departures" ]
            , input
                [ onInput ChangeQuery
                , onWithOptions "keydown" options dec
                , value model.query
                , autocomplete False
                , class "autocomplete-input"
                , placeholder "station"
                ]
                []
            , viewErrors model.fetchStationTableFailedMessage
            , viewAutocomplete model
            , viewAllDepartures model.departures
            ]


viewErrors : String -> Html Msg
viewErrors fetchStationTableFailedMessage =
    if String.isEmpty fetchStationTableFailedMessage then
        div [] []
    else
        div [] [ text fetchStationTableFailedMessage ]


viewAutocomplete : Model -> Html Msg
viewAutocomplete model =
    if model.showStations then
        div [ class "autocomplete-menu"]
            [ App.map SetAutoState (Autocomplete.view viewConfig model.howManyToShow model.autoState (acceptableStations model.query model.stations)) ]
    else
        div [] []


viewConfig : Autocomplete.ViewConfig Station
viewConfig =
    let
        stationListItem keySelected mouseSelected station =
            { attributes =
                [ classList [ ( "autocomplete-item", True ), ( "key-selected", keySelected ), ( "mouse-selected", mouseSelected ) ]
                , id station.name
                ]
            , children = [ Html.text station.name ]
            }
    in
        Autocomplete.viewConfig
            { toId = .name
            , ul = [ class "autocomplete-list" ]
            , li = stationListItem
            }


updateConfig : Autocomplete.UpdateConfig Msg Station
updateConfig =
    Autocomplete.updateConfig
        { toId = .name
        , onKeyDown =
            \code maybeId ->
                if code == 13 then
                    Maybe.map SelectStation maybeId
                else
                    Nothing
        , onTooLow = Just <| Wrap False
        , onTooHigh = Just <| Wrap True
        , onMouseEnter = \_ -> Nothing
        , onMouseLeave = \_ -> Nothing
        , onMouseClick = \id -> Just <| SelectStation id
        , separateSelections = False
        }


viewAllDepartures : List Departure -> Html.Html Msg
viewAllDepartures departures =
    if not (List.isEmpty departures) then
        table [ id "stationboard" ]
            [ thead []
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
    else
        div [] []


viewSingleDeparture : Departure -> Html.Html Msg
viewSingleDeparture departure =
    let
        departureTime =
            case Date.fromString departure.departure of
                Err msg ->
                    text ""

                Ok departure ->
                    text (toString (Date.hour departure) ++ ":" ++ toString (Date.minute departure))
    in
        tr []
            [ td [] [ departureTime ]
            , td [] [ text departure.name ]
            , td [] [ text departure.to ]
            ]
