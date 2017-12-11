module StationBoard exposing (init, update, view, subscriptions, Model, Msg)

import Autocomplete exposing (MouseSelected)
import Css exposing (center, marginLeft, textAlign)
import Date
import Date.Format
import Html
import Html.Attributes
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (align, attribute, autocomplete, class, classList, css, id, placeholder, style, styled, value)
import Html.Styled.Events exposing (keyCode, onClick, onFocus, onInput, onWithOptions)
import Http
import Json.Decode as Json exposing (field)
import List.Extra
import Styles exposing (..)
import OpenTransport.TransportApi as TransportApi exposing (..)
import OpenTransport.Station as Station exposing (..)


-- MODEL


type Mode
    = Search
    | Recent


type alias Model =
    { query : String
    , autoState : Autocomplete.State
    , stations : List Station
    , howManyToShow : Int
    , showStations : Bool
    , selectedStation : Maybe Station
    , departures : List Departure
    , fetchStationTableFailedMessage : String
    , latest : List Station
    , mode : Mode
    }


initialModel : Model
initialModel =
    Model
        ""
        Autocomplete.empty
        []
        5
        False
        Nothing
        []
        ""
        []
        Search


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Cmd.none
    )



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
    | ToggleMode
    | Clear


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Clear ->
            ( clear model, Cmd.none )

        SearchStation query ->
            ( { model | query = query }, getStations query )

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

        SelectStation name ->
            let
                selectedStation =
                    model.stations
                        |> List.filter (\station -> Station.name station == name)
                        |> List.head
            in
                ( selectStation model selectedStation name
                , getDepartures selectedStation
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
                            | autoState = Autocomplete.resetToLastItem updateConfig (acceptableStations model.query model.stations) model.howManyToShow model.autoState
                            , selectedStation =
                                List.head <|
                                    List.reverse <|
                                        List.take model.howManyToShow <|
                                            (acceptableStations model.query model.stations)
                        }
                            ! []
                    else
                        { model
                            | autoState = Autocomplete.resetToFirstItem updateConfig (acceptableStations model.query model.stations) model.howManyToShow model.autoState
                            , selectedStation =
                                List.head <|
                                    List.take model.howManyToShow <|
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
                        , showStations = True
                        , fetchStationTableFailedMessage = ""
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

        ToggleMode ->
            ( { model | mode = toggle model.mode }, Cmd.none )

        SelectStationFromRecent station ->
            ( model, getDepartures (Just station) )


toggle : Mode -> Mode
toggle mode =
    case mode of
        Search ->
            Recent

        Recent ->
            Search


clear : Model -> Model
clear { latest, mode } =
    { initialModel | latest = latest, mode = mode }


getStations : String -> Cmd Msg
getStations query =
    if (String.length query) >= 3 then
        query |> TransportApi.searchStation |> Http.send FetchStationSucceed
    else
        Cmd.none


getDepartures : Maybe Station -> Cmd Msg
getDepartures maybeStation =
    case maybeStation of
        Just station ->
            TransportApi.getDepartures (Station.name station) |> Http.send FetchStationTableSucceed

        Nothing ->
            Cmd.none


selectStation : Model -> Maybe Station -> String -> Model
selectStation model selectedStation id =
    { model
        | query =
            model.stations
                |> List.filter (\station -> Station.name station == id)
                |> List.head
                |> Maybe.withDefault (Station.empty)
                |> Station.name
        , autoState = Autocomplete.empty
        , showStations = False
        , selectedStation = selectedStation
        , latest = addStation model.latest selectedStation |> List.take 5
    }


addStation : List Station -> Maybe Station -> List Station
addStation stations maybeStation =
    case maybeStation of
        Just station ->
            (station :: stations)
                |> List.Extra.uniqueBy (\station -> Station.name station)

        Nothing ->
            stations


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
            , case model.mode of
                Search ->
                    viewSearchBar model.query

                Recent ->
                    viewRecentlySelected model.latest
            , viewErrors model.fetchStationTableFailedMessage
            , viewAutocomplete model
            , viewDepartures model.departures
            ]


viewTitle : Html msg
viewTitle =
    div []
        [ Styles.title [] [ text "Next departures from..." ]
        ]


viewSearchBar : String -> Html Msg
viewSearchBar searchString =
    div [ css [ textAlign center ] ]
        [ actionButton [ onClick ToggleMode ] [ text "Show recent searches" ]
        , searchField
            [ onInput SearchStation
            , value searchString
            , autocomplete False
            , placeholder "search  station..."
            ]
            []
        , clearButton [ onClick Clear ] [ text "x" ]
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
            [ actionButton [ onClick ToggleMode ] [ text "text search" ]
            , recentSearches
            ]


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
            Autocomplete.view viewConfig model.howManyToShow model.autoState (acceptableStations model.query model.stations)
    in
        if model.showStations then
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
        [ td [ cellStyle ] [ departureTime departure ]
        , td [ cellStyle ] [ text departure.name ]
        , td [ cellStyle ] [ text departure.to ]
        ]


departureTime : { a | departure : String } -> Html msg
departureTime departure =
    case Date.fromString departure.departure of
        Err msg ->
            text ""

        Ok departure ->
            text (Date.Format.format "%k:%M" departure)


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
