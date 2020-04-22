module OpenTransport.Departure exposing
    ( Departure
    , decode
    , viewDepartures
    )

import Element exposing (..)
import Element.Background as Background
import Iso8601
import Json.Decode as Json
import Style.Color exposing (grey)
import Time


type Departure
    = Departure
        { to : String
        , departure : Time.Posix
        , category : String
        , number : String
        }


create : String -> Time.Posix -> String -> String -> Departure
create to departure category number =
    Departure
        { to = to
        , departure = departure
        , category = category
        , number = number
        }



-- VIEW


viewDepartures : Time.Zone -> List Departure -> Element msg
viewDepartures timeZone departures =
    case departures of
        [] ->
            none

        xs ->
            column
                [ width fill ]
                (List.map (viewDeparture timeZone) xs)


viewDeparture : Time.Zone -> Departure -> Element msg
viewDeparture timeZone (Departure { category, number, departure, to }) =
    row
        [ width fill
        , padding 10
        , mouseOver [ Background.color grey ]
        ]
        [ viewCategory category
        , viewNumber number
        , viewDestination to
        , viewTime timeZone departure
        ]


viewCategory : String -> Element msg
viewCategory category =
    el [ alignLeft ] (text category)


viewNumber : String -> Element msg
viewNumber number =
    el [ alignLeft ] (text number)


viewDestination : String -> Element msg
viewDestination destination =
    el [ centerX ] (text destination)


viewTime : Time.Zone -> Time.Posix -> Element msg
viewTime timeZone departureTime =
    el [ alignRight ] (text (toString timeZone departureTime))


toString : Time.Zone -> Time.Posix -> String
toString zone posix =
    String.join ":"
        [ Time.toHour zone posix |> String.fromInt |> String.padLeft 2 '0'
        , Time.toMinute zone posix |> String.fromInt |> String.padLeft 2 '0'
        ]



-- Decoders


decode : Json.Decoder (List Departure)
decode =
    Json.map identity (Json.field "stationboard" (Json.list decodeDeparture))


decodeDeparture : Json.Decoder Departure
decodeDeparture =
    Json.map4
        create
        (Json.field "to" Json.string)
        (Json.at [ "stop", "departure" ] Iso8601.decoder)
        (Json.field "category" Json.string)
        (Json.field "number" Json.string)



