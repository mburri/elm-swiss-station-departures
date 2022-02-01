port module StationBoard exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Events
import Http
import Json.Decode as Json
import List.Extra
import OpenTransport.Departure exposing (Departure, viewDepartures)
import OpenTransport.Station as Station exposing (Station)
import OpenTransport.TransportApi as TransportApi exposing (..)
import Style.Color exposing (grey)
import Task
import Time
import ZipList exposing (ZipList)



-- ports


port setStorage : List String -> Cmd msg



-- MODEL


type alias Model =
    { query : String
    , stations : ZipList Station
    , recent : ZipList Station
    , selectedStation : Maybe Station

    -- model it as Loading | Loaded (List Departure) | Error String
    , departures : List Departure
    , fetchStationTableFailedMessage : String
    , timeZone : Time.Zone
    }


initialModel : List Station -> Model
initialModel recentStations =
    let
        recents =
            case recentStations of
                [] ->
                    ZipList.empty

                x :: xs ->
                    ZipList.new x xs
    in
    { query = ""
    , stations = recents
    , recent = recents
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
    | KeyUp Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchStation query ->
            searchStations model query

        SelectStation station ->
            selectStation model (Just station)

        FetchedDepartures result ->
            departuresFetched model result

        FetchedStations result ->
            stationsFetched model result

        GotTimeZone zone ->
            ( { model | timeZone = zone }, Cmd.none )

        KeyUp code ->
            case code of
                38 ->
                    ( { model | stations = ZipList.back model.stations }, Cmd.none )

                40 ->
                    ( { model | stations = ZipList.forward model.stations }, Cmd.none )

                13 ->
                    model.stations |> ZipList.current |> selectStation model

                _ ->
                    ( model, Cmd.none )


searchStations : Model -> String -> ( Model, Cmd Msg )
searchStations model query =
    if String.length query == 0 then
        ( { model
            | departures = []
            , stations = model.recent
            , query = query
          }
        , Cmd.none
        )

    else
        ( { model | query = query }
        , fetchStations query
        )


selectStation : Model -> Maybe Station -> ( Model, Cmd Msg )
selectStation model maybeSelected =
    let
        recents =
            case maybeSelected of
                Just selected ->
                    ZipList.toList model.recent
                        |> addStation selected
                        |> List.take 5

                Nothing ->
                    ZipList.toList model.recent

        newRecents =
            case recents of
                x :: xs ->
                    ZipList.new x xs

                _ ->
                    ZipList.empty

        newQuery =
            maybeSelected |> Maybe.map Station.stationName |> Maybe.withDefault ""
    in
    ( { model | recent = newRecents, query = newQuery }
    , Cmd.batch
        [ getDepartures (ZipList.current model.stations)
        , newRecents |> ZipList.toList |> List.map Station.stationName |> setStorage
        ]
    )


addStation : Station -> List Station -> List Station
addStation station stations =
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
                , stations = ZipList.empty
                , fetchStationTableFailedMessage = ""
              }
            , Cmd.none
            )

        Result.Err _ ->
            ( { model | fetchStationTableFailedMessage = "Error retrieving departures" }, Cmd.none )


stationsFetched : Model -> Result Http.Error (List Station) -> ( Model, Cmd msg )
stationsFetched model result =
    case result of
        Result.Ok stations ->
            let
                stationsZipList =
                    case stations of
                        [] ->
                            ZipList.empty

                        x :: xs ->
                            ZipList.new x xs
            in
            ( { model
                | stations = stationsZipList
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


view : Model -> Browser.Document Msg
view model =
    Browser.Document
        "Departures"
        [ Element.layout [] (viewStyled model)
        ]


viewStyled : Model -> Element Msg
viewStyled model =
    Element.column
        [ Element.width fill
        , Element.height fill
        , Element.centerX
        , Element.paddingXY 10 0
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
    case model.departures of
        [] ->
            model.stations
                |> ZipList.toList
                |> List.map (viewStation (ZipList.current model.stations))
                |> Element.column
                    [ Element.padding 5
                    , Element.width Element.fill
                    , Border.color (Element.rgb 0.9 0.9 0.9)
                    , Border.width 1
                    , Border.rounded 5
                    ]

        _ ->
            Element.none


onKeyUp : (Int -> msg) -> Element.Attribute msg
onKeyUp tagger =
    Html.Events.on "keyup" (Json.map tagger Html.Events.keyCode) |> Element.htmlAttribute


viewStation : Maybe Station -> Station -> Element Msg
viewStation current station =
    let
        highlighted =
            case current of
                Nothing ->
                    []

                Just element ->
                    if element == station then
                        [ Background.color grey ]

                    else
                        []

        attrs =
            [ Element.padding 5
            , Element.width Element.fill
            , Element.mouseOver [ Background.color grey ]
            , Events.onClick (SelectStation station)
            ]
                ++ highlighted
    in
    station
        |> Station.stationName
        |> Element.text
        |> Element.el attrs


viewSearchBar : Model -> Element Msg
viewSearchBar model =
    Element.column
        [ Element.width Element.fill
        , onKeyUp KeyUp
        ]
        [ Input.search
            [ Element.below (viewStations model)
            , Input.focusedOnLoad
            ]
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
        Http.BadUrl _ ->
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

        Http.BadPayload _ stringResponseHttp ->
            "Bad Payload - unable to handle response from server"
