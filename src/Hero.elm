module Hero exposing (..)

import Html exposing (Html, div, img, text)
import Json.Decode exposing (Decoder, field, int, map3, nullable, string)


type alias HeroID =
    Int


type alias Hero =
    { localizedName : String
    , imgUrl : String
    , id : HeroID
    }


decoder : Decoder Hero
decoder =
    map3 Hero
        (field "localized_name" string)
        (field "img" string)
        (field "id" int)


render : Hero -> Html msg
render hero =
    div []
        [ text hero.localizedName ]
