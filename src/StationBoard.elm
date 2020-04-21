port module StationBoard exposing (Model, Msg, document, init, subscriptions, update)

import Browser
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html
import Http
import List.Extra
import OpenTransport.Departure exposing (Departure, viewDepartures)
import OpenTransport.Station as Station exposing (Station)
import OpenTransport.TransportApi as TransportApi exposing (..)
import Style.Color exposing (grey)
import Task
import Time



-- ports


port setStorage : List String -> Cmd msg



-- MODEL


type alias Model =
    { query : String
    , stations : List Station
    , recent : List Station
    , selectedStation : Maybe Station
    , departures : List Departure
    , fetchStationTableFailedMessage : String
    , timeZone : Time.Zone
    }


initialModel : List Station -> Model
initialModel recentStations =
    { query = ""
    , stations = []
    , recent = recentStations
    , selectedStation = Nothing
    , departures = []
    , fetchStationTableFailedMessage = ""
    , timeZone = Time.utc
    }


init : List String -> ( Model, Cmd Msg )
init recentStations =
    let
        recent =
            List.map Station.create recentStations
    in
    ( initialModel recent, Task.perform GotTimeZone Time.here )



-- UPDATE


type Msg
    = SearchStation String
    | SelectStation Station
    | FetchedDepartures (Result Http.Error (List Departure))
    | FetchedStations (Result Http.Error (List Station))
    | GotTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchStation query ->
            searchStations model query

        SelectStation station ->
            updateSelectStation model station

        FetchedDepartures result ->
            departuresFetched model result

        FetchedStations result ->
            stationsFetched model result

        GotTimeZone zone ->
            ( { model | timeZone = zone }, Cmd.none )


searchStations : Model -> String -> ( Model, Cmd Msg )
searchStations model query =
    if String.length query == 0 then
        ( initialModel model.recent, Cmd.none )

    else
        ( { model | query = query }, fetchStations query )


updateSelectStation : Model -> Station -> ( Model, Cmd Msg )
updateSelectStation model selected =
    let
        newRecent =
            addStation model.recent selected
    in
    ( selectStation model newRecent selected
    , Cmd.batch
        [ getDepartures (Just selected)
        , newRecent |> List.map Station.stationName |> setStorage
        ]
    )


selectStation : Model -> List Station -> Station -> Model
selectStation model newRecent selected =
    { model
        | query = Station.stationName selected
        , selectedStation = Just selected
        , recent = newRecent |> List.take 5
    }


addStation : List Station -> Station -> List Station
addStation stations station =
    (station :: stations)
        |> List.Extra.uniqueBy (\s -> Station.stationName s)


getDepartures : Maybe Station -> Cmd Msg
getDepartures maybeStation =
    case maybeStation of
        Just station ->
            TransportApi.getDepartures (Station.stationName station) |> Http.send FetchedDepartures

        Nothing ->
            Cmd.none



-- HTTP


fetchStations : String -> Cmd Msg
fetchStations query =
    if String.length query >= 3 then
        query |> TransportApi.searchStation |> Http.send FetchedStations

    else
        Cmd.none


departuresFetched : Model -> Result Http.Error (List Departure) -> ( Model, Cmd msg )
departuresFetched model result =
    case result of
        Result.Ok departures ->
            ( { model
                | departures = departures
                , stations = []
                , fetchStationTableFailedMessage = ""
              }
            , Cmd.none
            )

        Result.Err err ->
            ( { model | fetchStationTableFailedMessage = "Error retrieving departures" }, Cmd.none )


stationsFetched : Model -> Result Http.Error (List Station) -> ( Model, Cmd msg )
stationsFetched model result =
    case result of
        Result.Ok stations ->
            ( { model
                | stations = stations
                , departures = []
                , fetchStationTableFailedMessage = ""
              }
            , Cmd.none
            )

        Result.Err err ->
            ( { model | fetchStationTableFailedMessage = toErrorMessage err }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


document : Model -> Browser.Document Msg
document model =
    Browser.Document
        "Departures"
        [ view model ]


view : Model -> Html.Html Msg
view model =
    Element.layout [] (viewStyled model)


viewStyled : Model -> Element Msg
viewStyled model =
    Element.column
        [ Element.centerX
        , Element.padding 50
        , Element.width Element.fill
        ]
        (viewHeader model
            ++ viewBody model
            ++ [ viewDepartures model.timeZone model.departures ]
        )


viewHeader : Model -> List (Element Msg)
viewHeader model =
    [ viewTitle
    , viewErrors model.fetchStationTableFailedMessage
    ]


viewTitle : Element msg
viewTitle =
    Element.row
        [ Element.centerX
        , Element.paddingXY 0 30
        , Font.size 48
        ]
        [ Element.text "Station Board" ]


viewBody : Model -> List (Element Msg)
viewBody model =
    [ viewSearchBar model ]


viewStations : Model -> Element Msg
viewStations model =
    let
        stations =
            case model.stations of
                [] ->
                    model.recent

                _ ->
                    model.stations
    in
    case model.departures of
        [] ->
            stations
                |> List.map viewStation
                |> Element.column
                    [ Element.padding 5
                    , Element.width Element.fill
                    , Border.color (Element.rgb 0.9 0.9 0.9)
                    , Border.width 1
                    , Border.rounded 5
                    ]

        _ ->
            Element.none


viewStation : Station -> Element Msg
viewStation station =
    station
        |> Station.stationName
        |> Element.text
        |> Element.el
            [ Element.padding 5
            , Element.width Element.fill
            , Element.mouseOver [ Background.color grey ]
            , Events.onClick (SelectStation station)
            ]


viewSearchBar : Model -> Element Msg
viewSearchBar model =
    Element.column [ Element.width Element.fill ]
        [ Input.search [ Element.below (viewStations model) ]
            { onChange = SearchStation
            , text = model.query
            , placeholder = Nothing
            , label = Input.labelHidden "Search"
            }
        ]


viewErrors : String -> Element Msg
viewErrors fetchStationTableFailedMessage =
    if String.isEmpty fetchStationTableFailedMessage then
        Element.none

    else
        Element.row []
            [ Element.text fetchStationTableFailedMessage ]


toErrorMessage : Http.Error -> String
toErrorMessage error =
    case error of
        Http.BadUrl string ->
            "Bad Url requested"

        Http.Timeout ->
            "Timeout - the server took too long to respond"

        Http.NetworkError ->
            "No network connection..."

        Http.BadStatus stringResponseHttp ->
            "HttpStatus "
                ++ String.fromInt stringResponseHttp.status.code
                ++ ", the message was: "
                ++ stringResponseHttp.status.message

        Http.BadPayload string stringResponseHttp ->
            "Bad Payload - unable to handle response from server"
