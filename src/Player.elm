module Player exposing (..)

import Json.Decode exposing (Decoder, field, int, map4, nullable, string)


type alias AccountID = Int


type alias Player =
    { accountId : AccountID
    , personaName : String
    , name : Maybe String
    , avatarUrl : String
    }


decoder : Decoder Player
decoder =
    map4 Player
        (field "account_id" int)
        (field "personaname" string)
        (field "name" (nullable string))
        (field "avatar" string)
