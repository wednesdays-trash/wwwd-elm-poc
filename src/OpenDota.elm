module OpenDota exposing (..)

import Hero exposing (Hero, HeroID)
import Http
import Json.Decode as Decode
    exposing
        ( Decoder
        , field
        , int
        , keyValuePairs
        , list
        , map4
        , nullable
        , string
        )
import Lane exposing (Lane)
import Match exposing (Match, MatchID)
import Patch exposing (Patch)
import Player exposing (Player)
import Url.Builder as U


type alias Matchup =
    { yourHero : Maybe HeroID
    , opposingHero : Maybe HeroID
    , lane : Lane
    }


baseurl =
    "https://api.opendota.com/api/"


rankings : Int -> (Result Http.Error (List Player) -> msg) -> Cmd msg
rankings heroId toMsg =
    Http.get
        { url = baseurl ++ "rankings?hero_id=" ++ String.fromInt heroId
        , expect = Http.expectJson toMsg (field "rankings" (list Player.decoder))
        }


allHeroes : (Result Http.Error (List Hero) -> msg) -> Cmd msg
allHeroes toMsg =
    Http.get
        { url = baseurl ++ "constants/heroes"
        , expect = Http.expectJson toMsg allHeroesDecoder
        }


allPatches : (Result Http.Error (List Patch) -> msg) -> Cmd msg
allPatches toMsg =
    Http.get
        { url = baseurl ++ "constants/patch"
        , expect = Http.expectJson toMsg (list Patch.decoder)
        }


heroSkilledPlayers : HeroID -> (Result Http.Error (List Player) -> msg) -> Cmd msg
heroSkilledPlayers heroId toMsg =
    Http.get
        { url = baseurl ++ "rankings?hero_id=" ++ String.fromInt heroId
        , expect = Http.expectJson toMsg (field "rankings" (list Player.decoder))
        }


playerMatches : Player -> HeroID -> HeroID -> Lane -> (Result Http.Error (List MatchID) -> msg) -> Cmd msg
playerMatches player yourHero opposingHero lane toMsg =
    let
        params =
            U.toQuery
                [ -- replays expire after 8 days according to this guy:
                  -- https://www.reddit.com/r/DotA2/comments/1lxug1/how_long_do_replays_last/cc3w1c5
                  U.int "date" 8
                , U.int "hero_id" yourHero
                , U.int "lane_role" (Lane.toID lane)
                , U.int "against_hero_id" opposingHero
                , U.string "project" "patch"
                ]
    in
    Http.get
        { url = baseurl ++ "players/" ++ String.fromInt player.accountId ++ "/matches" ++ params
        , expect = Http.expectJson toMsg (list Match.matchIdDecoder)
        }


match : MatchID -> (Result Http.Error Match -> msg) -> Cmd msg
match matchId toMsg =
    Http.get
        { url = baseurl ++ "matches/" ++ String.fromInt matchId
        , expect = Http.expectJson toMsg Match.decoder
        }


{-| Response for this endpoint is omega weird; it looks like:
{
"1": {...hero details...},
"2": {...hero details...},
"3": {...hero details...}
}
Since it's not a proper list, it requires this obnoxious decoder.
-}
allHeroesDecoder : Decoder (List Hero)
allHeroesDecoder =
    keyValuePairs Hero.decoder |> Decode.map (List.map Tuple.second)


makeMatchUrl : MatchID -> String
makeMatchUrl matchId =
    U.crossOrigin "https://opendota.com" [ "matches", String.fromInt matchId ] []
