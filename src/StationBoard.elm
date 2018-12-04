port module StationBoard exposing (Model, Msg, document, init, subscriptions, update)

import Browser
import Element exposing (Element)
import Element.Font as Font
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing (field)
import List.Extra
import Menu exposing (MouseSelected)
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
    | Nearby


type alias Model =
    { query : String
    , autoState : Menu.State
    , stations : List Station
    , selectedStation : Maybe Station
    , departures : List Departure
    , fetchStationTableFailedMessage : String
    , latest : List Station
    , mode : Mode
    }


initialModel : List Station -> Model
initialModel recent =
    Model
        ""
        Menu.empty
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
    = NoOp
    | SearchStation String
    | SetAutoState Menu.Msg
    | SelectStation String
    | SelectStationFromRecent Station
    | Wrap Bool
    | Reset
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
        NoOp ->
            ( model, Cmd.none )

        Clear ->
            ( clear model, Cmd.none )

        SearchStation query ->
            ( { model | query = query }, searchStations query )

        SetAutoState autoMsg ->
            let
                ( newState, maybeMsg ) =
                    Menu.update updateConfig autoMsg howManyToShow model.autoState (acceptableStations model.query model.stations)

                newModel =
                    { model | autoState = newState }
            in
            case maybeMsg of
                Nothing ->
                    ( newModel, Cmd.none )

                Just updateMsg ->
                    update updateMsg newModel

        SelectStation name ->
            let
                selectedStation =
                    model.stations
                        |> List.filter (\station -> Station.stationName station == name)
                        |> List.head

                newRecent =
                    addStation model.latest selectedStation
            in
            ( selectStation model newRecent selectedStation name
            , Cmd.batch
                [ getDepartures selectedStation
                , newRecent |> List.map Station.stationName |> setStorage
                ]
            )

        Reset ->
            ( { model
                | autoState = Menu.reset updateConfig model.autoState
                , selectedStation = Nothing
                , fetchStationTableFailedMessage = ""
              }
            , Cmd.none
            )

        Wrap toTop ->
            case model.selectedStation of
                Just station ->
                    update Reset model

                Nothing ->
                    if toTop then
                        ( { model
                            | autoState = Menu.resetToLastItem updateConfig (acceptableStations model.query model.stations) howManyToShow model.autoState
                            , selectedStation =
                                List.head <|
                                    List.reverse <|
                                        List.take howManyToShow <|
                                            acceptableStations model.query model.stations
                          }
                        , Cmd.none
                        )

                    else
                        ( { model
                            | autoState = Menu.resetToFirstItem updateConfig (acceptableStations model.query model.stations) howManyToShow model.autoState
                            , selectedStation =
                                List.head <|
                                    List.take howManyToShow <|
                                        acceptableStations model.query model.stations
                          }
                        , Cmd.none
                        )

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
                    let
                        _ =
                            Debug.log "Error retrieving departures" err
                    in
                    ( { model | fetchStationTableFailedMessage = "Error retrieving departures" }, Cmd.none )

        FetchStationSucceed result ->
            case result of
                Result.Ok stations ->
                    ( { model
                        | stations = stations
                        , fetchStationTableFailedMessage = ""
                        , autoState = Menu.resetToFirstItem updateConfig (acceptableStations model.query model.stations) howManyToShow model.autoState
                      }
                    , Cmd.none
                    )

                Result.Err err ->
                    let
                        _ =
                            Debug.log "Error retrieving stations" err
                    in
                    ( { model | fetchStationTableFailedMessage = toErrorMessage err }, Cmd.none )

        HandleEscape ->
            ( { model
                | query = ""
                , selectedStation = Nothing
                , departures = []
                , stations = []
                , autoState = Menu.empty
              }
            , Cmd.none
            )

        Switch mode ->
            ( { model | mode = mode }, Cmd.none )

        SelectStationFromRecent station ->
            ( model, getDepartures (Just station) )


clear : Model -> Model
clear { latest, mode } =
    let
        initial =
            initialModel latest
    in
    { initial | mode = mode }


selectStation : Model -> List Station -> Maybe Station -> String -> Model
selectStation model newRecent selectedStation id =
    { model
        | query =
            model.stations
                |> List.filter (\station -> Station.stationName station == id)
                |> List.head
                |> Maybe.withDefault Station.empty
                |> Station.stationName
        , autoState = Menu.empty
        , selectedStation = selectedStation
        , latest = newRecent |> List.take 5
    }


addStation : List Station -> Maybe Station -> List Station
addStation stations maybeStation =
    case maybeStation of
        Just station ->
            (station :: stations)
                |> List.Extra.uniqueBy (\s -> Station.stationName s)

        Nothing ->
            stations


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
    Sub.map SetAutoState Menu.subscription



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
                    if code == 38 || code == 40 then
                        Ok NoOp

                    else if code == 27 then
                        Ok HandleEscape

                    else
                        Err "not handling that key"
                )
    in
    Element.column [ Element.centerX ]
        [ viewTitle
        , Element.html (viewErrors model.fetchStationTableFailedMessage)
        , Element.html (viewButtons model.mode)
        , case model.mode of
            Search ->
                Element.html (viewSearchBar model)

            Recent ->
                Element.html (viewRecentlySelected model.latest)

            Nearby ->
                Element.html (viewRecentlySelected model.stations)
        , Element.html (viewDepartures model.departures)
        ]


viewTitle : Element msg
viewTitle =
    Element.row
        [ Element.padding 20
        , Font.size 48
        ]
        [ Element.text "Station Board" ]


viewButtons : a -> Html Msg
viewButtons model =
    div []
        [ button [ onClick (Switch Search) ] [ text "Search... " ]
        , button [ onClick (Switch Recent) ] [ text "Show recent searches" ]
        ]


viewSearchBar : Model -> Html Msg
viewSearchBar model =
    div []
        [ input
            [ onInput SearchStation
            , value model.query
            , autocomplete False
            , placeholder "search  station..."
            ]
            []
        , button [ onClick Clear ] [ text "x" ]
        , viewAutocomplete model
        ]


viewRecentlySelected : List Station -> Html Msg
viewRecentlySelected recents =
    let
        recentSearches =
            recents
                |> List.map viewRecent
                |> li []
    in
    div []
        [ recentSearches ]


viewRecent : Station -> Html Msg
viewRecent station =
    li [ onClick (SelectStationFromRecent station) ]
        [ station |> Station.stationName |> text ]


viewErrors : String -> Html Msg
viewErrors fetchStationTableFailedMessage =
    if String.isEmpty fetchStationTableFailedMessage then
        div [] []

    else
        div []
            [ text fetchStationTableFailedMessage ]


viewAutocomplete : Model -> Html Msg
viewAutocomplete model =
    let
        autocompleteView =
            Menu.view viewConfig howManyToShow model.autoState (acceptableStations model.query model.stations)

        showStationsMenu =
            not (List.isEmpty model.stations)
    in
    if showStationsMenu then
        div [ class "AutocompleteMenu" ]
            [ Html.map SetAutoState autocompleteView ]

    else
        div [] []


viewConfig : Menu.ViewConfig Station
viewConfig =
    let
        stationListItem keySelected mouseSelected station =
            { attributes =
                [ Html.Attributes.classList
                    [ ( "AutocompleteItem", True )
                    , ( "KeySelected", keySelected )
                    , ( "MouseSelected", mouseSelected )
                    ]
                , station |> Station.stationName |> Html.Attributes.id
                ]
            , children = [ station |> Station.stationName |> Html.text ]
            }
    in
    Menu.viewConfig
        { toId = Station.stationName
        , ul = [ Html.Attributes.class "AutocompleteList" ]
        , li = stationListItem
        }


updateConfig : Menu.UpdateConfig Msg Station
updateConfig =
    Menu.updateConfig
        { toId = Station.stationName
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


viewDepartures : List Departure -> Html msg
viewDepartures departures =
    if not (List.isEmpty departures) then
        table []
            [ thead []
                [ tr
                    []
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
        text ""


viewSingleDeparture : Departure -> Html msg
viewSingleDeparture departure =
    tr []
        [ td [] [ Departure.time departure |> text ]
        , td [] [ Departure.departureName departure |> text ]
        , td [] [ Departure.destination departure |> text ]
        ]


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
