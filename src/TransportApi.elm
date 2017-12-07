module TransportApi
    exposing
        ( Station
        , Departure
        , getDepartures
        , searchStation
        , emptyStation
        , stationName
        )

import Http
import Json.Decode as Json exposing (field)


type Station
    = Station
        { name : String
        }


emptyStation : Station
emptyStation =
    Station { name = "" }


stationName : Station -> String
stationName (Station { name }) =
    name


type alias Departure =
    { to : String
    , departure : String
    , name : String
    }


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


decodeStation : Json.Decoder Station
decodeStation =
    Json.map toStation (field "name" Json.string)


toStation : String -> Station
toStation name =
    Station { name = name }


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
    Json.map3 Departure
        (field "to" Json.string)
        (Json.at [ "stop", "departure" ] Json.string)
        (field "name" Json.string)
