module Station exposing (Station, decodeStations, acceptableStations)

import Json.Decode as Json exposing (field)


type alias Station =
    { name : String
    }


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
