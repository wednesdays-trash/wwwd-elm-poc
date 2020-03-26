module View exposing (..)

import Hero exposing (Hero, HeroID)
import Html exposing (Attribute, Html, option, select, text)
import Html.Attributes exposing (value, selected)
import Html.Events exposing (on, onClick)
import Lane exposing (Lane(..))
import Patch exposing (Patch)


heroesDropdown : List (Attribute msg) -> String -> List Hero -> (Maybe HeroID -> msg) -> Html msg
heroesDropdown attrs placeholder heroes toMsg =
    let
        renderHero hero =
            option
                [ value (String.fromInt hero.id), onClick (toMsg (Just hero.id)) ]
                [ text hero.localizedName ]

        defaultValue =
            option [ value "" ] [ text placeholder ]

        heroOptions =
            heroes |> List.sortBy .localizedName |> List.map renderHero
    in
    select attrs (defaultValue :: heroOptions)


lanesDropdown : (Lane -> msg) -> Html msg
lanesDropdown toMsg =
    select []
        [ option [ value "1", onClick (toMsg Safe) ] [ text "Safe" ]
        , option [ value "2", onClick (toMsg Mid), selected True ] [ text "Mid" ]
        , option [ value "3", onClick (toMsg Off) ] [ text "Off" ]
        ]
