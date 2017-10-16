module Main exposing (..)

import Autocomplete
import Date exposing (Date)
import Date.Format
import Departure exposing (Departure, decodeDepartures)
import Html exposing (Html, button, div, h1, input, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (align, attribute, autocomplete, class, classList, id, placeholder, style, value)
import Html.Events exposing (keyCode, onFocus, onInput, onWithOptions)
import Http
import Json.Decode as Json exposing (field)
import Station exposing (Station, decodeStations)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


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
    | FetchStationTableSucceed (Result Http.Error (List Departure))
    | FetchStationSucceed (Result Http.Error (List Station))
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
                , getDepartures selectedStation
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
            case result of
                Result.Ok departures ->
                    ( { model | departures = departures }, Cmd.none )

                Result.Err err ->
                    let
                        _ =
                            Debug.log "Error retrieving departures" err
                    in
                        ( model, Cmd.none )

        FetchStationSucceed result ->
            case result of
                Result.Ok stations ->
                    ( { model
                        | stations = stations
                        , showStations = True
                      }
                    , Cmd.none
                    )

                Result.Err err ->
                    let
                        _ =
                            Debug.log "Error retrieving stations" err
                    in
                        ( model, Cmd.none )

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
            "https://transport.opendata.ch/v1/locations?query=" ++ query
    in
        if String.length query >= 3 then
            Http.get url decodeStations |> Http.send FetchStationSucceed
        else
            Cmd.none


getDepartures : Maybe Station -> Cmd Msg
getDepartures maybeStation =
    case maybeStation of
        Just station ->
            let
                url =
                    "https://transport.opendata.ch/v1/stationboard?station=" ++ station.name ++ "&limit=20"
            in
                Http.get url decodeDepartures |> Http.send FetchStationTableSucceed

        Nothing ->
            Cmd.none


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


view : Model -> Html Msg
view model =
    let
        options =
            { preventDefault = True, stopPropagation = False }

        dec =
            -- TODO: naming?
            (Json.map
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
        div [ class "autocomplete-menu" ]
            [ Html.map SetAutoState (Autocomplete.view viewConfig model.howManyToShow model.autoState (acceptableStations model.query model.stations)) ]
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
                    text (Date.Format.format "%k:%M" departure)
    in
        tr []
            [ td [] [ departureTime ]
            , td [] [ text departure.name ]
            , td [] [ text departure.to ]
            ]
