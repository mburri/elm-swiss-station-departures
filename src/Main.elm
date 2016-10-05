module Main exposing (..)

import Autocomplete
import Html.App as App
import Html exposing (Html, button, div, text, h1, input)
import Html.Attributes exposing (id, classList, class, value, autocomplete, style)
import Html.Events exposing (onInput, onWithOptions, keyCode)
import Json.Decode as Json
import String
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


type alias Model =
    { query : String
    , autoState : Autocomplete.State
    , stations : List Station
    , howManyToShow : Int
    , showStations : Bool
    }


type Msg
    = NoOp
    | ChangeQuery String
    | SetAutoState Autocomplete.Msg
    | SelectStation String
    | PreviewStation String


init : ( Model, Cmd Msg )
init =
    ( Model
        ""
        Autocomplete.empty
        stations
        5
        False
    , Cmd.none
    )


acceptableStations : String -> List Station -> List Station
acceptableStations query stations =
    List.filter (matches query) stations


matches : String -> Station -> Bool
matches query station =
    String.contains (String.toLower query) (String.toLower station.name)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ChangeQuery q ->
            let
                hasMatches =
                    not <|
                        List.isEmpty <|
                            (acceptableStations q model.stations)

                emptyQuery =
                    String.isEmpty q
            in
                ( { model
                    | query = q
                    , showStations = hasMatches && not emptyQuery
                  }
                , Cmd.none
                )

        PreviewStation station ->
            -- TODO: implement me
            ( model, Cmd.none )

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

        SelectStation selectedId ->
            -- TODO: implement me
            let
                _ =
                    Debug.log "selected station" selectedId
            in
                ( model, Cmd.none )



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
                        --Ok HandleEscape
                        Ok NoOp
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
                ]
                []
            , viewAutocomplete model
            ]


viewAutocomplete : Model -> Html Msg
viewAutocomplete model =
    if model.showStations then
        div []
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
                if code == 38 || code == 40 then
                    Maybe.map PreviewStation maybeId
                else if code == 13 then
                    Maybe.map SelectStation maybeId
                else
                    Nothing
        , onTooLow = Nothing
        , onTooHigh = Nothing
        , onMouseEnter = \id -> Just <| PreviewStation id
        , onMouseLeave = \_ -> Nothing
        , onMouseClick = \id -> Just <| SelectStation id
        , separateSelections = False
        }



-- temp data


stations : List Station
stations =
    [ Station "Bern"
    , Station "Schliern bei Köniz"
    , Station "Köniz"
    , Station "Köniz Zentrum"
    , Station "Köniz Schloss"
    , Station "Eigerplatz, Bern"
    , Station "Eigerplatz, Thun"
    , Station "Eigergletscher"
    ]
