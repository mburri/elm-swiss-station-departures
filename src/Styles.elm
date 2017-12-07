module Styles exposing (..)

import Css exposing (..)
import Css.Media as Media
import Css.Colors
import Css.Foreign exposing (global)
import Css.Media exposing (only, screen, withMedia)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, styled)


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


title : List (Attribute msg) -> List (Html msg) -> Html msg
title =
    styled h1
        [ display block
        , color Css.Colors.white
        , fontSize (Css.rem 2.5)
        , textAlign center
        ]


rowStyle : Attribute msg
rowStyle =
    css
        [ color Css.Colors.white
        , borderBottom3 (px 1) solid Css.Colors.red
        ]


cellStyle : Attribute msg
cellStyle =
    css
        [ color Css.Colors.white
        , fontSize (Css.rem 1.5)
        , padding2 (Css.rem 0.5) (Css.rem 0.8)
        , borderBottom3 (px 1) solid theme.primary5
        ]



-- pre styled elements - ready to use


clearButton : List (Attribute msg) -> List (Html msg) -> Html msg
clearButton =
    styled button
        [ padding (Css.rem 0.5)
        , border3 (px 1) solid theme.primary4
        , fontSize (Css.rem 1.0)
        , fontWeight bold
        , color theme.primary4
        , backgroundColor Css.Colors.white
        , verticalAlign super
        , marginLeft (Css.rem -2.0)
        , hover
            [ backgroundColor theme.primary5
            ]
        ]


actionButton : List (Attribute msg) -> List (Html msg) -> Html msg
actionButton =
    styled button
        [ padding (Css.rem 0.5)
        , margin (Css.rem 0.5)
        , border3 (px 1) solid theme.primary4
        , fontSize (Css.rem 1.0)
        , fontWeight bold
        , color theme.primary4
        , backgroundColor Css.Colors.white
        , hover
            [ backgroundColor theme.primary5
            ]
        ]


searchField : List (Attribute msg) -> List (Html msg) -> Html msg
searchField =
    styled input
        [ fontSize (Css.rem 2.0)
        , width (pct 90)
        , margin auto
        , padding (Css.rem 0.5)
        , borderRadius (Css.rem 0.2)
        , backgroundColor Css.Colors.white
        , color theme.primary5
        ]


recentStationList : List (Attribute msg) -> List (Html msg) -> Html msg
recentStationList =
    styled ul
        [ fontSize (Css.rem 2.0)
        , color Css.Colors.white
        , listStyle none
        , padding (Css.rem 2.0)
        ]


recentStationListItem : List (Attribute msg) -> List (Html msg) -> Html msg
recentStationListItem =
    styled li
        [ margin auto
        , paddingLeft (Css.rem 2.0)
        , paddingRight (Css.rem 2.0)
        , width (pct 70)
        , borderBottom3 (px 1) solid theme.primary5
        ]


departuresTable : List (Attribute msg) -> List (Html msg) -> Html msg
departuresTable =
    styled Html.Styled.table
        [ margin (Css.rem 2.0)
        , Css.width (px 880)
        , borderCollapse collapse
        ]


errorBox : List (Attribute msg) -> List (Html msg) -> Html msg
errorBox =
    styled
        div
        [ backgroundColor theme.secondary1
        , color Css.Colors.white
        , margin (Css.rem 2.0)
        , padding (Css.rem 1.5)
        ]



-- todo: get rid of global styles asap.


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
            , withMedia [ only screen [ Media.minWidth (px 320) ] ]
                [ fontSize (px 30) ]
            ]
        , Css.Foreign.body
            [ maxWidth (px 960)
            , width (pct 80)
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
