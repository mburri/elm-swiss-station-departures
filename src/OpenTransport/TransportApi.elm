module OpenTransport.TransportApi exposing
    ( getDepartures
    , searchStation
    )

import Http
import Iso8601
import Json.Decode as Json exposing (field)
import OpenTransport.Departure as Departure exposing (Departure)
import OpenTransport.Station as Station exposing (Station)


baseUrl : String
baseUrl =
    "https://transport.opendata.ch/v1"


searchStation : String -> Http.Request (List Station)
searchStation query =
    let
        url =
            baseUrl ++ "/locations?query=" ++ query
    in
    Http.get url decodeStations



-- nearestStations : Location -> Http.Request (List Station)
-- nearestStations { latitude, longitude } =
--     let
--         ( lat, long ) =
--             ( String.fromFloat latitude, String.fromFloat longitude )
--         url =
--             baseUrl ++ "/locations?x=" ++ lat ++ "&y=" ++ long
--     in
--     Http.get url decodeStations


decodeStation : Json.Decoder Station
decodeStation =
    Json.map Station.create (field "name" Json.string)


decodeStations : Json.Decoder (List Station)
decodeStations =
    Json.map identity
        (field "stations" (Json.list decodeStation))


getDepartures : String -> Http.Request (List Departure)
getDepartures stationName =
    let
        url =
            baseUrl ++ "/stationboard?station=" ++ stationName ++ "&limit=20"
    in
    Http.get url decode


decode : Json.Decoder (List Departure)
decode =
    Json.map identity (field "stationboard" (Json.list decodeDeparture))


decodeDeparture : Json.Decoder Departure
decodeDeparture =
    Json.map3 Departure.create
        (field "to" Json.string)
        (Json.at [ "stop", "departure" ] Iso8601.decoder)
        (field "name" Json.string)