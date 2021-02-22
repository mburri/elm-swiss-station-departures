module OpenTransport.Station exposing
    ( Station
    , create
    , stationName
    )


type Station
    = Station
        { name : String
        }


stationName : Station -> String
stationName (Station { name }) =
    name


create : String -> Station
create name =
    Station { name = name }
