port module StationBoard exposing (init, update, view, subscriptions, Model, Msg)

import Autocomplete exposing (MouseSelected)
import Css exposing (center, marginLeft, textAlign)
import Geolocation exposing (Location)
import Html
import Html.Attributes
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (align, attribute, autocomplete, class, classList, css, id, placeholder, style, styled, value)
import Html.Styled.Events exposing (keyCode, onClick, onFocus, onInput, onWithOptions)
import Http
import Json.Decode as Json exposing (field)
import List.Extra
import OpenTransport.Departure as Departure exposing (Departure, time)
import OpenTransport.Station as Station exposing (Station)
import OpenTransport.TransportApi as TransportApi exposing (..)
import Styles exposing (..)
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
    , autoState : Autocomplete.State
    , stations : List Station
    , selectedStation : Maybe Station
    , departures : List Departure
    , fetchStationTableFailedMessage : String
    , latest : List Station
    , mode : Mode
    , location : Maybe Location
    }


initialModel : List Station -> Model
initialModel recent =
    Model
        ""
        Autocomplete.empty
        []
        Nothing
        []
        ""
        recent
        Search
        Nothing


init : List String -> ( Model, Cmd Msg )
init recentStations =
    let
        recent =
            List.map Station.create recentStations
    in
        ( initialModel recent, Task.attempt GetLocation Geolocation.now )



-- UPDATE


type Msg
    = NoOp
    | SearchStation String
    | SetAutoState Autocomplete.Msg
    | SelectStation String
    | SelectStationFromRecent Station
    | Wrap Bool
    | Reset
    | FetchStationTableSucceed (Result Http.Error (List Departure))
    | FetchStationSucceed (Result Http.Error (List Station))
    | HandleEscape
    | Switch Mode
    | Clear
    | Nearest
    | GetLocation (Result Geolocation.Error Location)


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
                    Autocomplete.update updateConfig autoMsg howManyToShow model.autoState (acceptableStations model.query model.stations)

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
                        |> List.filter (\station -> Station.name station == name)
                        |> List.head

                newRecent =
                    addStation model.latest selectedStation
            in
                ( selectStation model newRecent selectedStation name
                , Cmd.batch
                    [ getDepartures selectedStation
                    , newRecent |> List.map Station.name |> setStorage
                    ]
                )

        Reset ->
            { model
                | autoState = Autocomplete.reset updateConfig model.autoState
                , selectedStation = Nothing
                , fetchStationTableFailedMessage = ""
            }
                ! []

        Wrap toTop ->
            case model.selectedStation of
                Just station ->
                    update Reset model

                Nothing ->
                    if toTop then
                        { model
                            | autoState = Autocomplete.resetToLastItem updateConfig (acceptableStations model.query model.stations) howManyToShow model.autoState
                            , selectedStation =
                                List.head <|
                                    List.reverse <|
                                        List.take howManyToShow <|
                                            (acceptableStations model.query model.stations)
                        }
                            ! []
                    else
                        { model
                            | autoState = Autocomplete.resetToFirstItem updateConfig (acceptableStations model.query model.stations) howManyToShow model.autoState
                            , selectedStation =
                                List.head <|
                                    List.take howManyToShow <|
                                        (acceptableStations model.query model.stations)
                        }
                            ! []

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
                        ( { model | fetchStationTableFailedMessage = toString err }, Cmd.none )

        FetchStationSucceed result ->
            case result of
                Result.Ok stations ->
                    ( { model
                        | stations = stations
                        , fetchStationTableFailedMessage = ""
                        , autoState = Autocomplete.resetToFirstItem updateConfig (acceptableStations model.query model.stations) howManyToShow model.autoState
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
                , autoState = Autocomplete.empty
              }
            , Cmd.none
            )

        Switch mode ->
            ( { model | mode = mode }, Cmd.none )

        SelectStationFromRecent station ->
            ( model, getDepartures (Just station) )

        GetLocation (Err err) ->
            ( model, Cmd.none )

        GetLocation (Result.Ok location) ->
            ( { model | location = Just location }, Cmd.none )

        Nearest ->
            case model.location of
                Just location ->
                    ( { model | mode = Nearby }, getNearbyStations location )

                Nothing ->
                    ( model, Task.attempt GetLocation Geolocation.now )


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
                |> List.filter (\station -> Station.name station == id)
                |> List.head
                |> Maybe.withDefault (Station.empty)
                |> Station.name
        , autoState = Autocomplete.empty
        , selectedStation = selectedStation
        , latest = newRecent |> List.take 5
    }


addStation : List Station -> Maybe Station -> List Station
addStation stations maybeStation =
    case maybeStation of
        Just station ->
            (station :: stations)
                |> List.Extra.uniqueBy (\station -> Station.name station)

        Nothing ->
            stations


searchStations : String -> Cmd Msg
searchStations query =
    if (String.length query) >= 3 then
        query |> TransportApi.searchStation |> Http.send FetchStationSucceed
    else
        Cmd.none


getNearbyStations : Location -> Cmd Msg
getNearbyStations location =
    location |> TransportApi.nearestStations |> Http.send FetchStationSucceed


getDepartures : Maybe Station -> Cmd Msg
getDepartures maybeStation =
    case maybeStation of
        Just station ->
            TransportApi.getDepartures (Station.name station) |> Http.send FetchStationTableSucceed

        Nothing ->
            Cmd.none


acceptableStations : String -> List Station -> List Station
acceptableStations query stations =
    List.filter (matches query) stations


matches : String -> Station -> Bool
matches query station =
    String.contains (String.toLower query) (String.toLower (Station.name station))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map SetAutoState Autocomplete.subscription



-- VIEW


view : Model -> Html.Html Msg
view model =
    toUnstyled (viewStyled model)


viewStyled : Model -> Html Msg
viewStyled model =
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
            [ globalStyles
            , viewTitle
            , viewErrors model.fetchStationTableFailedMessage
            , viewButtons model.mode
            , case model.mode of
                Search ->
                    viewSearchBar model

                Recent ->
                    viewRecentlySelected model.latest

                Nearby ->
                    viewRecentlySelected model.stations
            , viewDepartures model.departures
            ]


viewTitle : Html msg
viewTitle =
    div []
        [ Styles.title [] [ text "Next departures from..." ]
        ]


viewButtons : a -> Html Msg
viewButtons model =
    div [ css [ textAlign center ] ]
        [ actionButton [ onClick (Switch Search) ] [ text "Search... " ]
        , actionButton [ onClick (Switch Recent) ] [ text "Show recent searches" ]
        , actionButton [ onClick Nearest ] [ text "Nearby stations" ]
        ]


viewSearchBar : Model -> Html Msg
viewSearchBar model =
    div []
        [ searchField
            [ onInput SearchStation
            , value model.query
            , autocomplete False
            , placeholder "search  station..."
            ]
            []
        , clearButton [ onClick Clear ] [ text "x" ]
        , viewAutocomplete model
        ]


viewRecentlySelected : List Station -> Html Msg
viewRecentlySelected recents =
    let
        recentSearches =
            recents
                |> List.map viewRecent
                |> recentStationList []
    in
        div [ css [ textAlign center ] ]
            [ recentSearches ]


viewRecent : Station -> Html Msg
viewRecent station =
    recentStationListItem
        [ onClick (SelectStationFromRecent station) ]
        [ station |> Station.name |> text ]


viewErrors : String -> Html Msg
viewErrors fetchStationTableFailedMessage =
    if String.isEmpty fetchStationTableFailedMessage then
        div [] []
    else
        errorBox []
            [ text fetchStationTableFailedMessage ]


viewAutocomplete : Model -> Html Msg
viewAutocomplete model =
    let
        autocompleteView =
            Autocomplete.view viewConfig howManyToShow model.autoState (acceptableStations model.query model.stations)

        showStationsMenu =
            not (List.isEmpty model.stations)
    in
        if showStationsMenu then
            div [ class "AutocompleteMenu" ]
                [ Html.Styled.map SetAutoState (fromUnstyled autocompleteView) ]
        else
            div [] []


viewConfig : Autocomplete.ViewConfig Station
viewConfig =
    let
        stationListItem keySelected mouseSelected station =
            { attributes =
                [ Html.Attributes.classList
                    [ ( "AutocompleteItem", True )
                    , ( "KeySelected", keySelected )
                    , ( "MouseSelected", mouseSelected )
                    ]
                , station |> Station.name |> Html.Attributes.id
                ]
            , children = [ station |> Station.name |> Html.text ]
            }
    in
        Autocomplete.viewConfig
            { toId = Station.name
            , ul = [ Html.Attributes.class "AutocompleteList" ]
            , li = stationListItem
            }


updateConfig : Autocomplete.UpdateConfig Msg Station
updateConfig =
    Autocomplete.updateConfig
        { toId = Station.name
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


viewDepartures : List Departure -> Html.Styled.Html msg
viewDepartures departures =
    if not (List.isEmpty departures) then
        departuresTable []
            [ thead []
                [ tr
                    [ rowStyle
                    ]
                    [ th [ cellStyle, align "left" ]
                        [ text "Zeit" ]
                    , th [ cellStyle ]
                        [ text "" ]
                    , th [ cellStyle, align "left" ]
                        [ text "Nach" ]
                    ]
                ]
            , tbody [] (List.map viewSingleDeparture departures)
            ]
    else
        text ""


viewSingleDeparture : Departure -> Html.Styled.Html msg
viewSingleDeparture departure =
    tr []
        [ td [ cellStyle ] [ Departure.time departure |> text ]
        , td [ cellStyle ] [ Departure.name departure |> text ]
        , td [ cellStyle ] [ Departure.destination departure |> text ]
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
                ++ toString (stringResponseHttp.status.code)
                ++ ", the message was: "
                ++ stringResponseHttp.status.message

        Http.BadPayload string stringResponseHttp ->
            "Bad Payload - unable to handle response from server"
