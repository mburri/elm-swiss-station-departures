module OpenTransport.Departure exposing (Departure, create, destination, name, time)

import Date
import Date.Format


type Departure
    = Departure
        { to : String
        , departure : String
        , name : String
        }


create : String -> String -> String -> Departure
create to departure name =
    Departure
        { to = to
        , departure = departure
        , name = name
        }


destination : Departure -> String
destination (Departure { to }) =
    to


name : Departure -> String
name (Departure { name }) =
    name


time : Departure -> String
time (Departure { departure }) =
    case Date.fromString departure of
        Err msg ->
            "-"

        Ok departure ->
            Date.Format.format "%k:%M" departure
