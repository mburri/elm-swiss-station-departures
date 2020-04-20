module OpenTransport.Departure exposing
    ( Departure
    , create
    , viewDepartures
    )

import Element exposing (Element)
import Element.Background as Background
import Style.Color exposing (grey)


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



-- VIEW


viewDepartures : List Departure -> Element msg
viewDepartures departures =
    case departures of
        [] ->
            Element.none

        xs ->
            Element.column
                [ Element.width Element.fill ]
                (List.map viewDeparture xs)


viewDeparture : Departure -> Element msg
viewDeparture (Departure { departure, name, to }) =
    Element.row
        [ Element.width Element.fill
        , Element.padding 5
        , Element.mouseOver [ Background.color grey ]
        ]
        [ Element.el [ Element.width (Element.fillPortion 3) ] (viewTime departure)
        , Element.el [ Element.width (Element.fillPortion 1) ] (Element.text name)
        , Element.el [ Element.width (Element.fillPortion 3) ] (Element.text to)
        ]


viewTime : String -> Element msg
viewTime departureTime =
    Element.text departureTime
