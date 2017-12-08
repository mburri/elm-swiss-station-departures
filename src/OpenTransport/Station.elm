module OpenTransport.Station exposing (..)


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


toStation : String -> Station
toStation name =
    Station { name = name }
