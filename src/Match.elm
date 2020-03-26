module Match exposing (..)

import Dict exposing (Dict)
import Hero exposing (HeroID)
import Json.Decode as J exposing (Decoder, bool, field, int, list, map2, map3, string)
import Lane exposing (Lane)
import Patch exposing (PatchID)


type alias MatchID =
    Int


matchIdDecoder : Decoder MatchID
matchIdDecoder =
    field "match_id" int


type alias Match =
    { id : MatchID
    , patch : PatchID
    , heroes : Dict HeroID HeroInMatch
    }


decoder : Decoder Match
decoder =
    let
        makeDictEntry hero =
            ( hero.id, hero )

        toDict =
            List.map makeDictEntry >> Dict.fromList
    in
    map3 Match
        matchIdDecoder
        (field "patch" int)
        (field "players" (list heroInMatchDecoder) |> J.map toDict)


type alias HeroInMatch =
    { id : HeroID
    , lane : Lane
    }


heroInMatchDecoder : Decoder HeroInMatch
heroInMatchDecoder =
    map2 HeroInMatch
        (field "hero_id" int)
        (field "lane" Lane.decoder)


isHeroInAnyOfTheseLanes : HeroID -> List Lane -> Match -> Bool
isHeroInAnyOfTheseLanes heroId lanes match =
    let
        heroInMatch =
            Dict.get heroId match.heroes
    in
    case heroInMatch of
        Just hero ->
            List.member hero.lane lanes

        Nothing ->
            False
