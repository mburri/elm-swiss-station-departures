module StationBoard exposing (init, update, view, subscriptions, Model, Msg)

import Autocomplete exposing (MouseSelected)
import Css exposing (..)
import Css.Colors
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
import TransportApi exposing (..)


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
        []
        Search
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

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
                        |> List.filter (\station -> station.name == name)
                        |> List.head
            in
                ( selectStation model selectedStation name
                , getOpenTransportApis selectedStation
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
            ( model, getOpenTransportApis (Just station) )


toggle : Mode -> Mode
toggle mode =
    case mode of
        Search ->
            Recent

        Recent ->
            Search


getStations : String -> Cmd Msg
getStations query =
    if (String.length query) >= 3 then
        query |> TransportApi.searchStation |> Http.send FetchStationSucceed
    else
        Cmd.none


getOpenTransportApis : Maybe Station -> Cmd Msg
getOpenTransportApis maybeStation =
    case maybeStation of
        Just station ->
            TransportApi.getDepartures station.name |> Http.send FetchStationTableSucceed

        Nothing ->
            Cmd.none


selectStation : Model -> Maybe Station -> String -> Model
selectStation model selectedStation id =
    { model
        | query =
            model.stations
                |> List.filter (\station -> station.name == id)
                |> List.head
                |> Maybe.withDefault (Station "")
                |> .name
        , autoState = Autocomplete.empty
        , showStations = False
        , selectedStation = selectedStation
        , latest = addStation model.latest selectedStation
    }


addStation : List Station -> Maybe Station -> List Station
addStation stations maybeStation =
    case maybeStation of
        Just station ->
            (station :: stations)
                |> List.Extra.uniqueBy (\station -> station.name)

        Nothing ->
            stations


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
    div [ css [ textAlign center ] ]
        [ h1
            [ css
                [ display block
                , color Css.Colors.white
                , fontSize (Css.rem 2.5)
                ]
            ]
            [ text "Next departures from..." ]
        ]


modeButton : List (Attribute msg) -> List (Html msg) -> Html msg
modeButton =
    styled button
        [ padding (Css.rem 0.5)
        , marginBottom (Css.rem 0.5)
        , border3 (px 1) solid theme.primary3
        , fontSize (Css.rem 1.0)
        , fontWeight bold
        , color theme.primary5
        , backgroundColor theme.secondary5
        ]


viewSearchBar : String -> Html Msg
viewSearchBar searchString =
    div [ css [ textAlign center ] ]
        [ modeButton [ onClick ToggleMode ] [ text "Show recent searches" ]
        , input
            [ css
                [ fontSize (Css.rem 2.0)
                , width (pct 90)
                , margin auto
                , padding (Css.rem 0.5)
                , borderRadius (Css.rem 0.2)
                , backgroundColor Css.Colors.white
                , color theme.primary5
                ]
            , onInput SearchStation
            , value searchString
            , autocomplete False
            , placeholder "search  station..."
            ]
            []
        ]


viewRecentlySelected : List Station -> Html Msg
viewRecentlySelected recents =
    let
        recentSearches =
            recents
                |> List.map viewRecent
                |> ul
                    [ css
                        [ fontSize (Css.rem 2.0)
                        , color Css.Colors.white
                        , listStyle none
                        , padding (Css.rem 2.0)
                        ]
                    ]
    in
        div [ css [ textAlign center ] ]
            [ modeButton [ onClick ToggleMode ] [ text "text search" ]
            , recentSearches
            ]


viewRecent : Station -> Html Msg
viewRecent station =
    li
        [ css
            [ margin auto
            , paddingLeft (Css.rem 2.0)
            , paddingRight (Css.rem 2.0)
            , width (pct 30)
            , borderBottom3 (px 1) solid theme.primary5
            ]
        , onClick (SelectStationFromRecent station)
        ]
        [ text station.name ]


viewErrors : String -> Html Msg
viewErrors fetchStationTableFailedMessage =
    if String.isEmpty fetchStationTableFailedMessage then
        div [] []
    else
        div
            [ css
                [ backgroundColor theme.secondary1
                , color Css.Colors.white
                , margin (Css.rem 2.0)
                , padding (Css.rem 1.5)
                ]
            ]
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
                , Html.Attributes.id station.name
                ]
            , children = [ Html.text station.name ]
            }
    in
        Autocomplete.viewConfig
            { toId = .name
            , ul = [ Html.Attributes.class "AutocompleteList" ]
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


cellStyle : Attribute msg
cellStyle =
    css
        [ color Css.Colors.white
        , fontSize (Css.rem 1.5)
        , padding2 (Css.rem 0.5) (Css.rem 0.8)
        , borderBottom3 (px 1) solid theme.primary5
        ]


viewDepartures : List Departure -> Html.Styled.Html msg
viewDepartures departures =
    if not (List.isEmpty departures) then
        Html.Styled.table
            [ css
                [ margin (Css.rem 2.0)
                , Css.width (px 880)
                , borderCollapse collapse
                ]
            ]
            [ thead []
                [ tr
                    [ css
                        [ color Css.Colors.white
                        , borderBottom3 (px 1) solid Css.Colors.red
                        ]
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
    let
        departureTime =
            case Date.fromString departure.departure of
                Err msg ->
                    text ""

                Ok departure ->
                    text (Date.Format.format "%k:%M" departure)
    in
        tr []
            [ td [ cellStyle ] [ departureTime ]
            , td [ cellStyle ] [ text departure.name ]
            , td [ cellStyle ] [ text departure.to ]
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
