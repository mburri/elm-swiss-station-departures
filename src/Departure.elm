module Departure exposing (Departure, decode, view, get)

import Http
import Css exposing (..)
import Css.Colors
import Date
import Date.Format
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Json.Decode as Json exposing (field)
import Theme exposing (theme)


type alias Departure =
    { to : String
    , departure : String
    , name : String
    }


get : String -> Http.Request (List Departure)
get stationName =
    let
        url =
            "https://transport.opendata.ch/v1/stationboard?station=" ++ stationName ++ "&limit=20"
    in
        Http.get url decode


decode : Json.Decoder (List Departure)
decode =
    Json.map identity (field "stationboard" (Json.list decodeDeparture))


decodeDeparture : Json.Decoder Departure
decodeDeparture =
    Json.map3 Departure
        (field "to" Json.string)
        (Json.at [ "stop", "departure" ] Json.string)
        (field "name" Json.string)


cellStyle : Attribute msg
cellStyle =
    css
        [ color Css.Colors.white
        , fontSize (Css.rem 1.5)
        , padding2 (Css.rem 0.5) (Css.rem 0.8)
        , borderBottom3 (px 1) solid theme.primary5
        ]


view : List Departure -> Html.Styled.Html msg
view departures =
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
