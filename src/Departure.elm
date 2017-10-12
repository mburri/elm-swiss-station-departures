module Departure exposing (Departure, decodeDepartures)

import Json.Decode as Json exposing (field)


type alias Departure =
    { to : String
    , departure : String
    , name : String
    }


decodeDepartures : Json.Decoder (List Departure)
decodeDepartures =
    Json.map identity (field "stationboard" (Json.list decodeDeparture))


decodeDeparture : Json.Decoder Departure
decodeDeparture =
    Json.map3 Departure
        (field "to" Json.string)
        (Json.at [ "stop", "departure" ] Json.string)
        (field "name" Json.string)
