port module StationBoard exposing (Model, Msg, document, init, subscriptions, update)

import Browser
import Element exposing (Element)
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing (field)
import List.Extra
import OpenTransport.Departure as Departure exposing (Departure, time)
import OpenTransport.Station as Station exposing (Station)
import OpenTransport.TransportApi as TransportApi exposing (..)
import Task



-- ports


port setStorage : List String -> Cmd msg



-- MODEL


type Mode
    = Search
    | Recent


type alias Model =
    { query : String
    , stations : List Station
    , selectedStation : Maybe Station
    , departures : List Departure
    , fetchStationTableFailedMessage : String
    , recent : List Station
    , mode : Mode
    }


initialModel : List Station -> Model
initialModel recent =
    Model
        ""
        []
        Nothing
        []
        ""
        recent
        Search


init : List String -> ( Model, Cmd Msg )
init recentStations =
    let
        recent =
            List.map Station.create recentStations
    in
    ( initialModel recent, Cmd.none )



-- UPDATE


type Msg
    = SearchStation String
    | SelectStation Station
    | FetchStationTableSucceed (Result Http.Error (List Departure))
    | FetchStationSucceed (Result Http.Error (List Station))
    | HandleEscape
    | Switch Mode
    | Clear


howManyToShow : number
howManyToShow =
    5


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clear ->
            ( clear model, Cmd.none )

        SearchStation query ->
            ( { model | query = query }, searchStations query )

        SelectStation station ->
            updateSelectStation model station

        FetchStationTableSucceed result ->
            case result of
                Result.Ok departures ->
                    ( { model
                        | departures = departures
                        , fetchStationTableFailedMessage = ""
                      }
                    , Cmd.none
                    )

                Result.Err err ->
                    ( { model | fetchStationTableFailedMessage = "Error retrieving departures" }, Cmd.none )

        FetchStationSucceed result ->
            case result of
                Result.Ok stations ->
                    ( { model
                        | stations = stations
                        , fetchStationTableFailedMessage = ""
                      }
                    , Cmd.none
                    )

                Result.Err err ->
                    ( { model | fetchStationTableFailedMessage = toErrorMessage err }, Cmd.none )

        HandleEscape ->
            ( clear model, Cmd.none )

        Switch mode ->
            ( { model | mode = mode }, Cmd.none )


clear : Model -> Model
clear { recent, mode } =
    let
        initial =
            initialModel recent
    in
    { initial | mode = mode }


updateSelectStation : Model -> Station -> ( Model, Cmd Msg )
updateSelectStation model selected =
    let
        newRecent =
            case model.mode of
                Search ->
                    addStation model.recent selected

                Recent ->
                    model.recent
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


searchStations : String -> Cmd Msg
searchStations query =
    if String.length query >= 3 then
        query |> TransportApi.searchStation |> Http.send FetchStationSucceed

    else
        Cmd.none


getDepartures : Maybe Station -> Cmd Msg
getDepartures maybeStation =
    case maybeStation of
        Just station ->
            TransportApi.getDepartures (Station.stationName station) |> Http.send FetchStationTableSucceed

        Nothing ->
            Cmd.none


acceptableStations : String -> List Station -> List Station
acceptableStations query stations =
    List.filter (matches query) stations


matches : String -> Station -> Bool
matches query station =
    String.contains (String.toLower query) (String.toLower (Station.stationName station))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


document : Model -> Browser.Document Msg
document model =
    Browser.Document "Swiss Departures" [ view model ]


view : Model -> Html.Html Msg
view model =
    Element.layout [] (viewStyled model)


viewStyled : Model -> Element Msg
viewStyled model =
    let
        options =
            { preventDefault = True, stopPropagation = False }

        dec =
            -- TODO: naming?
            Json.map
                (\code ->
                    if code == 27 then
                        Ok HandleEscape

                    else
                        Err "not handling that key"
                )
    in
    Element.column
        [ Element.centerX
        , Element.padding 50
        , Element.width Element.fill
        ]
        (viewHeader model ++ viewBody model ++ [ viewDepartures model.departures ])


viewHeader : Model -> List (Element Msg)
viewHeader model =
    [ viewTitle
    , viewErrors model.fetchStationTableFailedMessage
    , viewButtons model.mode
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
    case model.mode of
        Search ->
            [ viewSearchBar model ]

        Recent ->
            [ viewRecentlySelected model.recent ]


viewButtons : a -> Element Msg
viewButtons model =
    Element.row
        [ Element.centerX
        , Element.width Element.fill
        , Element.spaceEvenly
        ]
        [ Input.button [ Element.centerX, Element.padding 50 ] { onPress = Just (Switch Search), label = Element.text "Search" }
        , Input.button [ Element.centerX, Element.padding 50 ] { onPress = Just (Switch Recent), label = Element.text "Recent" }
        ]


viewSearchBar : Model -> Element Msg
viewSearchBar model =
    Element.column [ Element.width Element.fill ]
        [ Input.search []
            { onChange = SearchStation
            , text = model.query
            , placeholder = Nothing
            , label = Input.labelHidden "Search"
            }
        , viewAutocomplete model
        ]


viewRecentlySelected : List Station -> Element Msg
viewRecentlySelected recents =
    let
        recentSearches =
            recents
                |> List.map (viewRecent recents)
    in
    Element.column [] recentSearches


viewRecent : List Station -> Station -> Element Msg
viewRecent recents station =
    Element.row [ Events.onClick (SelectStation station) ] [ station |> Station.stationName |> Element.text ]


viewErrors : String -> Element Msg
viewErrors fetchStationTableFailedMessage =
    if String.isEmpty fetchStationTableFailedMessage then
        Element.none

    else
        Element.row []
            [ Element.text fetchStationTableFailedMessage ]


viewAutocomplete : Model -> Element Msg
viewAutocomplete model =
    let
        showStationsMenu =
            not (List.isEmpty model.stations)
    in
    if showStationsMenu then
        Element.row [] []

    else
        Element.none


viewDepartures : List Departure -> Element msg
viewDepartures departures =
    if List.isEmpty departures then
        Element.none

    else
        Element.table []
            { data = departures
            , columns =
                [ { header = Element.text "Zeit"
                  , width = Element.fill
                  , view = \departure -> Element.text (Departure.time departure)
                  }
                , { header = Element.none
                  , width = Element.fill
                  , view = \departure -> Element.text (Departure.departureName departure)
                  }
                , { header = Element.text "Nach"
                  , width = Element.fill
                  , view = \departure -> Element.text (Departure.destination departure)
                  }
                ]
            }


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
