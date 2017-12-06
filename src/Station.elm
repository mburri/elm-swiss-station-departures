module Station exposing (Station, decodeStations, acceptableStations, search)

import Http
import Json.Decode as Json exposing (field)


type alias Station =
    { name : String
    }


search : String -> Http.Request (List Station)
search query =
    let
        url =
            "https://transport.opendata.ch/v1/locations?query=" ++ query
    in
        Http.get url decodeStations


decodeStation : Json.Decoder Station
decodeStation =
    Json.map Station (field "name" Json.string)


decodeStations : Json.Decoder (List Station)
decodeStations =
    Json.map identity
        (field "stations" (Json.list decodeStation))


acceptableStations : String -> List Station -> List Station
acceptableStations query stations =
    List.filter (matches query) stations


matches : String -> Station -> Bool
matches query station =
    String.contains (String.toLower query) (String.toLower station.name)
