module Patch exposing (..)

import Html exposing (Html, div, img, text)
import Json.Decode exposing (Decoder, field, int, map3, nullable, string)


type alias PatchID =
    Int


type alias Patch =
    { name : String
    , date : String
    , id : PatchID
    }


decoder : Decoder Patch
decoder =
    map3 Patch
        (field "name" string)
        (field "date" string)
        (field "id" int)


render : Patch -> Html msg
render patch =
    div []
        [ text patch.name
        , text patch.date
        ]
