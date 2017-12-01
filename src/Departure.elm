module Departure exposing (Departure, decode, view)

import Date
import Html.Styled exposing (div, table, thead, tbody, tr, th, td, text)
import Html.Styled.Attributes exposing (align, id)
import Json.Decode as Json exposing (field)
import Date.Format


type alias Departure =
    { to : String
    , departure : String
    , name : String
    }


decode : Json.Decoder (List Departure)
decode =
    Json.map identity (field "stationboard" (Json.list decodeDeparture))


decodeDeparture : Json.Decoder Departure
decodeDeparture =
    Json.map3 Departure
        (field "to" Json.string)
        (Json.at [ "stop", "departure" ] Json.string)
        (field "name" Json.string)


view : List Departure -> Html.Styled.Html msg
view departures =
    if not (List.isEmpty departures) then
        table [ id "stationboard" ]
            [ thead []
                [ tr []
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
        div [] []


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
            [ td [] [ departureTime ]
            , td [] [ text departure.name ]
            , td [] [ text departure.to ]
            ]
