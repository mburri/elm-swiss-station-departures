module OpenTransport.Station exposing
    ( Station
    , create
    , empty
    , stationName
    )


type Station
    = Station
        { name : String
        }


empty : Station
empty =
    Station { name = "" }


stationName : Station -> String
stationName (Station { name }) =
    name


create : String -> Station
create name =
    Station { name = name }
