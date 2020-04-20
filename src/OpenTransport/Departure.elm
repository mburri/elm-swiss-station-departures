module OpenTransport.Departure exposing
    ( Departure
    , create
    , viewDepartures
    )

import Element exposing (Element)
import Element.Background as Background
import Style.Color exposing (grey)
import Time


type Departure
    = Departure
        { to : String
        , departure : Time.Posix
        , name : String
        }


create : String -> Time.Posix -> String -> Departure
create to departure stationName =
    Departure
        { to = to
        , departure = departure
        , name = stationName
        }



-- VIEW


viewDepartures : Time.Zone -> List Departure -> Element msg
viewDepartures timeZone departures =
    case departures of
        [] ->
            Element.none

        xs ->
            Element.column
                [ Element.width Element.fill ]
                (List.map (viewDeparture timeZone) xs)


viewDeparture : Time.Zone -> Departure -> Element msg
viewDeparture timeZone (Departure { departure, name, to }) =
    Element.row
        [ Element.width Element.fill
        , Element.padding 5
        , Element.mouseOver [ Background.color grey ]
        ]
        [ Element.el [ Element.width (Element.fillPortion 3) ] (viewTime timeZone departure)
        , Element.el [ Element.width (Element.fillPortion 1) ] (Element.text name)
        , Element.el [ Element.width (Element.fillPortion 3) ] (Element.text to)
        ]


viewTime : Time.Zone -> Time.Posix -> Element msg
viewTime timeZone departureTime =
    Element.text (toString timeZone departureTime)


toString : Time.Zone -> Time.Posix -> String
toString zone posix =
    String.join " "
        [ (Time.toWeekday zone posix |> weekdayToName) ++ ","
        , (Time.toDay zone posix |> String.fromInt) ++ "."
        , Time.toMonth zone posix |> monthToName
        , Time.toYear zone posix |> String.fromInt
        , String.join ":"
            [ Time.toHour zone posix |> String.fromInt |> String.padLeft 2 '0'
            , Time.toMinute zone posix |> String.fromInt |> String.padLeft 2 '0'
            , Time.toSecond zone posix |> String.fromInt |> String.padLeft 2 '0'
            ]
        ]


monthToName : Time.Month -> String
monthToName m =
    case m of
        Time.Jan ->
            "January"

        Time.Feb ->
            "February"

        Time.Mar ->
            "March"

        Time.Apr ->
            "April"

        Time.May ->
            "May"

        Time.Jun ->
            "June"

        Time.Jul ->
            "July"

        Time.Aug ->
            "August"

        Time.Sep ->
            "September"

        Time.Oct ->
            "October"

        Time.Nov ->
            "November"

        Time.Dec ->
            "December"


weekdayToName : Time.Weekday -> String
weekdayToName wd =
    case wd of
        Time.Mon ->
            "Monday"

        Time.Tue ->
            "Tuesday"

        Time.Wed ->
            "Wednesday"

        Time.Thu ->
            "Thursday"

        Time.Fri ->
            "Friday"

        Time.Sat ->
            "Saturday"

        Time.Sun ->
            "Sunday"
