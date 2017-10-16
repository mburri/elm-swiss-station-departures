module Station exposing (Station, decodeStations)

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
