module Main exposing (main)

import Browser
import StationBoard exposing (Model, Msg, init, subscriptions, update, view)


main : Program (List String) Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
