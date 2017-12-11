module OpenTransport.Station exposing (Station, empty, name, create)


type Station
    = Station
        { name : String
        }


empty : Station
empty =
    Station { name = "" }


name : Station -> String
name (Station { name }) =
    name


create : String -> Station
create name =
    Station { name = name }
