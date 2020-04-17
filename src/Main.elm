module Main exposing (main)

import Browser
import StationBoard exposing (..)


main : Program (List String) Model Msg
main =
    Browser.document
        { init = init
        , view = document
        , update = update
        , subscriptions = subscriptions
        }
