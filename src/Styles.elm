module Styles exposing (..)

import Css exposing (..)
import Css.Colors
import Css.Foreign exposing (global)
import Html.Styled exposing (..)


theme :
    { primary1 : Color
    , primary2 : Color
    , primary3 : Color
    , primary4 : Color
    , secondary1 : Color
    , secondary2 : Color
    , secondary3 : Color
    , secondary4 : Color
    , secondary5 : Color
    , primary5 : Color
    }
theme =
    { primary1 = hex "#003399"
    , primary2 = hex "#333399"
    , primary3 = hex "#003366"
    , primary4 = hex "#3366CC"
    , primary5 = hex "#6699CC"
    , secondary1 = hex "#FF9900"
    , secondary2 = hex "#CC9933"
    , secondary3 = hex "#996600"
    , secondary4 = hex "#FFCC33"
    , secondary5 = hex "#FFCC66"
    }


type Styles
    = KeySelected
    | MouseSelected
    | AutocompleteMenu
    | AutocompleteList
    | AutocompleteItem


globalStyles : Html msg
globalStyles =
    global
        [ Css.Foreign.html
            [ fontSize (px 20)
            , width (pct 100)
            ]
        , Css.Foreign.body
            [ width (px 960)
            , margin auto
            , fontFamily sansSerif
            , backgroundColor theme.primary1
            ]
        , Css.Foreign.class KeySelected
            [ backgroundColor theme.primary3
            ]
        , Css.Foreign.class MouseSelected
            [ backgroundColor theme.primary3 ]
        , Css.Foreign.class AutocompleteMenu
            [ margin (Css.rem 2.0)
            , color Css.Colors.white
            ]
        , Css.Foreign.class AutocompleteItem
            [ display block
            , padding2 (Css.rem 0.3) (Css.rem 0.8)
            , fontSize (Css.rem 1.5)
            , borderBottom3 (px 1) solid theme.primary5
            , cursor pointer
            ]
        , Css.Foreign.class AutocompleteList
            [ listStyle none
            , padding (px 0)
            , margin auto
            , maxHeight (Css.rem 12)
            , overflowY auto
            ]
        ]
