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


type alias Departure =
    { to : String
    , departure : Time.Posix
    , name : String
    , category : String
    , number : String
    }



-- VIEW


viewDepartures : Time.Zone -> List Departure -> Element msg
viewDepartures timeZone departures =
    case departures of
        [] ->
            Element.none

        xs ->
            Element.column
                [ Element.width Element.fill ]
                (List.map (viewDeparture timeZone) xs)


viewDeparture : Time.Zone -> Departure -> Element msg
viewDeparture timeZone { category, number, departure, to } =
    Element.row
        [ Element.width Element.fill
        , Element.padding 10
        , Element.mouseOver [ Background.color grey ]
        ]
        [ viewCategory category
        , viewNumber number
        , viewDestination to
        , viewTime timeZone departure
        ]


viewCategory : String -> Element msg
viewCategory category =
    el [ alignLeft ] (text category)


viewNumber number =
    el [ alignLeft ] (text number)


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


decode : Json.Decoder Departure
decode =
    Json.map5 Departure
        (Json.field "to" Json.string)
        (Json.at [ "stop", "departure" ] Iso8601.decoder)
        (Json.field "name" Json.string)
        (Json.field "category" Json.string)
        (Json.field "number" Json.string)
