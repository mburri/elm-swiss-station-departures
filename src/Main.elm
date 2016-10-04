module Main exposing (..)

import Autocomplete
import Html exposing (Html, button, div, text, h1, input)
import Html.App as App
import Html.Attributes exposing (id, classList, class)
import Html.Events exposing (onInput)
import String


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

        SetAutoState msg ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view model =
    div []
        [ h1 [] [ text "elm-swiss-station-departures" ]
        , input [ onInput ChangeQuery ] []
        , viewAutocomplete model
        ]


viewAutocomplete : Model -> Html Msg
viewAutocomplete model =
    if model.showStations then
        div [ class "autocomplete-menu" ]
            [ App.map SetAutoState (Autocomplete.view viewConfig model.howManyToShow model.autoState (acceptableStations model.query model.stations)) ]
    else
        text ""


viewConfig : Autocomplete.ViewConfig Station
viewConfig =
    let
        stationListItem keySelected mouseSelected station =
            { attributes = []
            , children = [ Html.text station.name ]
            }
    in
        Autocomplete.viewConfig
            { toId = .name
            , ul = [ class "autocomplete-list" ]
            , li = stationListItem
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
