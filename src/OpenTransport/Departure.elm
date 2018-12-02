module OpenTransport.Departure exposing
    ( Departure
    , create
    , departureName
    , destination
    , time
    )


type Departure
    = Departure
        { to : String
        , departure : String
        , name : String
        }


create : String -> String -> String -> Departure
create to departure stationName =
    Departure
        { to = to
        , departure = departure
        , name = stationName
        }


destination : Departure -> String
destination (Departure { to }) =
    to


departureName : Departure -> String
departureName (Departure { name }) =
    name


time : Departure -> String
time (Departure { departure }) =
    {--todo: format date --}
    departure
