module Main exposing (main)

import Html exposing (Html, button, div, text, h1, table, colgroup, col, thead, tbody, th, tr, td, input)
import Html.App as App
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (align, attribute, id, placeholder)


main : Program Never
main =
    App.beginnerProgram { model = init, view = view, update = update }



-- Model


type alias Model =
    { query : String
    }


init : Model
init =
    { query = "" }



-- Update


type Msg
    = NoOp
    | ChangeQuery String


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        ChangeQuery query ->
            { model | query = query }



-- View


view : Model -> Html.Html Msg
view model =
    div []
        [ h1 [] [ text "Swiss Station Departures" ]
        , div []
            [ input [ placeholder "Station", onInput ChangeQuery ]
                []
            ]
        , div []
            [ text model.query ]
        , table [ id "stationboard" ]
            [ colgroup []
                [ col [ attribute "width" "120" ]
                    []
                , col [ attribute "width" "140" ]
                    []
                , col [ attribute "width" "230" ]
                    []
                ]
            , thead []
                [ tr []
                    [ th [ align "left" ]
                        [ text "Zeit" ]
                    , th []
                        [ text "" ]
                    , th [ align "left" ]
                        [ text "Nach" ]
                    ]
                ]
            , tbody []
                [ tr []
                    [ td []
                        [ text "14:22" ]
                    , td []
                        [ text "NFB 28" ]
                    , td []
                        [ text "Bern Wankdorf, Bahnhof" ]
                    ]
                , tr []
                    [ td []
                        [ text "14:24" ]
                    , td []
                        [ text "NFB 10" ]
                    , td []
                        [ text "Schliern bei Köniz" ]
                    ]
                , tr []
                    [ td []
                        [ text "14:25" ]
                    , td []
                        [ text "NFB 10" ]
                    , td []
                        [ text "Ostermundigen, Rüti" ]
                    ]
                , tr []
                    [ td []
                        [ text "14:29" ]
                    , td []
                        [ text "NFB 10" ]
                    , td []
                        [ text "Schliern bei Köniz" ]
                    ]
                , tr []
                    [ td []
                        [ text "14:30" ]
                    , td []
                        [ text "NFB 10" ]
                    , td []
                        [ text "Ostermundigen, Rüti" ]
                    ]
                , tr []
                    [ td []
                        [ text "14:34" ]
                    , td []
                        [ text "NFB 10" ]
                    , td []
                        [ text "Schliern bei Köniz" ]
                    ]
                , tr []
                    [ td []
                        [ text "14:35" ]
                    , td []
                        [ text "NFB 10" ]
                    , td []
                        [ text "Ostermundigen, Rüti" ]
                    ]
                , tr []
                    [ td []
                        [ text "14:35" ]
                    , td []
                        [ text "10" ]
                    , td []
                        [ text "Ostermundigen" ]
                    ]
                , tr []
                    [ td []
                        [ text "14:37" ]
                    , td []
                        [ text "NFB 28" ]
                    , td []
                        [ text "Bern Wankdorf, Bahnhof" ]
                    ]
                , tr []
                    [ td []
                        [ text "14:39" ]
                    , td []
                        [ text "NFB 10" ]
                    , td []
                        [ text "Schliern bei Köniz" ]
                    ]
                , tr []
                    [ td []
                        [ text "14:40" ]
                    , td []
                        [ text "NFB 10" ]
                    , td []
                        [ text "Ostermundigen, Rüti" ]
                    ]
                , tr []
                    [ td []
                        [ text "14:44" ]
                    , td []
                        [ text "NFB 10" ]
                    , td []
                        [ text "Schliern bei Köniz" ]
                    ]
                , tr []
                    [ td []
                        [ text "14:45" ]
                    , td []
                        [ text "NFB 10" ]
                    , td []
                        [ text "Ostermundigen, Rüti" ]
                    ]
                , tr []
                    [ td []
                        [ text "14:49" ]
                    , td []
                        [ text "NFB 10" ]
                    , td []
                        [ text "Schliern bei Köniz" ]
                    ]
                , tr []
                    [ td []
                        [ text "14:50" ]
                    , td []
                        [ text "NFB 10" ]
                    , td []
                        [ text "Ostermundigen, Rüti" ]
                    ]
                ]
            ]
        ]
