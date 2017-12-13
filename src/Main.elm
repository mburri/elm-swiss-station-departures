module Main exposing (main)

import Html
import OpenTransport.Station exposing (Station)
import StationBoard exposing (..)


main : Program (List String) Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
