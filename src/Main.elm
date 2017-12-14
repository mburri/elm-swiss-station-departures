module Main exposing (main)

import Html
import StationBoard exposing (..)


main : Program (List String) Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
