module Main exposing (..)

-- TODO replace Debug with proper handling

import Browser
import Dict exposing (Dict)
import Hero exposing (Hero, HeroID)
import Html exposing (Html, a, button, div, label, li, p, text, ul)
import Html.Attributes exposing (disabled, href)
import Html.Events exposing (onClick)
import Http exposing (Error(..))
import Lane exposing (Lane(..))
import Match exposing (Match, MatchID)
import OpenDota exposing (Matchup)
import Patch exposing (Patch)
import Player exposing (Player)
import Util
import View


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias Error =
    Http.Error


type Status
    = Alright
    | Failure Error


type alias Model =
    { status : Status
    , heroes : List Hero
    , patches : List Patch
    , results : List MatchID
    , matchup : Matchup
    , skilledPlayers : Dict HeroID (List Player)

    -- matches where it isn't certain the opposing hero is on the same lane, we need to
    -- fetch all of this match's details (from /match rather than /players/XXX/matches)
    -- to figure this out
    , potentialMatches : Dict HeroID (List MatchID)
    , matches : Dict HeroID (List Match)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { status = Alright
      , heroes = []
      , patches = []
      , results = []
      , matchup =
            { yourHero = Nothing
            , opposingHero = Nothing
            , lane = Mid
            }
      , skilledPlayers = Dict.empty
      , potentialMatches = Dict.empty
      , matches = Dict.empty
      }
    , Cmd.batch [ OpenDota.allHeroes ReceivedHeroes, OpenDota.allPatches ReceivedPatches ]
    )


type Msg
    = ReceivedHeroes (Result Http.Error (List Hero))
    | ReceivedPatches (Result Http.Error (List Patch))
    | ReceivedSkilledPlayers HeroID (Result Http.Error (List Player))
    | ReceivedMatchIDs HeroID HeroID Lane (Result Http.Error (List MatchID))
    | ReceivedMatch HeroID HeroID Lane (Result Http.Error Match)
    | ChangedMatchup Matchup
    | Fetch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedHeroes result ->
            case result of
                Ok heroes ->
                    ( { model | heroes = heroes }, Cmd.none )

                Err e ->
                    ( { model | status = Failure e }, Cmd.none )

        ReceivedPatches result ->
            case result of
                Ok patches ->
                    ( { model | patches = patches }, Cmd.none )

                Err e ->
                    ( { model | status = Failure e }, Cmd.none )

        ReceivedSkilledPlayers heroId requestResult ->
            case requestResult of
                Ok players ->
                    ( { model | skilledPlayers = Dict.insert heroId players model.skilledPlayers }
                    , Cmd.none
                    )

                Err e ->
                    ( { model | status = Failure e }, Cmd.none )

        Fetch ->
            ( model, OpenDota.allHeroes ReceivedHeroes )

        ReceivedMatchIDs yourHeroId opposingHeroId lane requestResult ->
            let
                appendMatchIDs matchIDs currentMatches =
                    case currentMatches of
                        Just ms ->
                            Just (List.append ms matchIDs)

                        Nothing ->
                            Just matchIDs

                requestMatch matchId =
                    OpenDota.match matchId (ReceivedMatch yourHeroId opposingHeroId lane)
            in
            case requestResult of
                Ok matchIDs ->
                    ( { model
                        | potentialMatches =
                            Dict.update yourHeroId (appendMatchIDs matchIDs) model.potentialMatches
                      }
                    , Cmd.batch (List.map requestMatch matchIDs)
                    )

                Err e ->
                    ( { model | status = Failure e }, Cmd.none )

        ReceivedMatch yourHeroId opposingHeroId lane requestResult ->
            let
                appendMatch match currentMatches =
                    case currentMatches of
                        Just ms ->
                            Just (match :: ms)

                        Nothing ->
                            Just [ match ]

                newModel match =
                    if Debug.log "is in any of these lanes" (Match.isHeroInAnyOfTheseLanes opposingHeroId (Lane.opposing lane) match) then
                        { model | matches = Dict.update yourHeroId (appendMatch match) model.matches }

                    else
                        model
            in
            case requestResult of
                Ok match ->
                    ( newModel match, Cmd.none )

                Err e ->
                    ( { model | status = Failure e }, Cmd.none )

        ChangedMatchup matchup ->
            case matchup.yourHero of
                Just yourHero ->
                    case Dict.get yourHero model.skilledPlayers of
                        -- this hero's top players were already fetched
                        Just playersOfHero ->
                            case matchup.opposingHero of
                                Just opposingHero ->
                                    ( { model | matchup = matchup }
                                    , playersOfHero
                                        |> List.take 15 -- TODO change back to 30
                                        |> List.map
                                            (\p ->
                                                OpenDota.playerMatches p
                                                    yourHero
                                                    opposingHero
                                                    matchup.lane
                                                    (ReceivedMatchIDs yourHero opposingHero matchup.lane)
                                            )
                                        |> Cmd.batch
                                    )

                                Nothing ->
                                    ( { model | matchup = matchup }, Cmd.none )

                        -- this hero's top players weren't fetched yet
                        Nothing ->
                            ( { model | matchup = matchup }
                            , OpenDota.heroSkilledPlayers yourHero (ReceivedSkilledPlayers yourHero)
                            )

                Nothing ->
                    ( { model | matchup = matchup }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        currentMatchup =
            model.matchup

        playersFetched =
            model.matchup.yourHero
                |> Maybe.andThen (\h -> Dict.get h model.skilledPlayers)
                |> Util.isJust

        yourHeroDropdown =
            View.heroesDropdown
                []
                "Choose your hero"
                model.heroes
                (\heroId -> ChangedMatchup { currentMatchup | yourHero = heroId })

        opponentDropdownAttrs =
            if playersFetched then
                []

            else
                [ disabled True ]

        opponentDropdownPlaceholderText =
            if playersFetched then
                "Choose an opponent"

            else
                "Pick your hero first ;)"

        opponentDropdown =
            View.heroesDropdown opponentDropdownAttrs
                opponentDropdownPlaceholderText
                model.heroes
                (\heroId -> ChangedMatchup { currentMatchup | opposingHero = heroId })

        players =
            model.matchup.yourHero
                |> Maybe.andThen (\h -> Dict.get h model.skilledPlayers)
                |> Maybe.map (\ps -> List.map (\x -> p [] [ text x.personaName ]) ps)

        renderMatch match =
            li [] [ a [ href (OpenDota.makeMatchUrl match.id) ] [ text (String.fromInt match.id) ] ]

        matches =
            model.matchup.yourHero
                |> Maybe.andThen (\h -> Dict.get h model.matches)
                |> Maybe.map (List.map renderMatch)
    in
    div []
        [ yourHeroDropdown
        , text " vs "
        , opponentDropdown
        , text " on "
        , View.lanesDropdown (\lane -> ChangedMatchup { currentMatchup | lane = lane })
        , ul [] (Maybe.withDefault [] matches)
        , case model.status of
            Alright ->
                div [] []

            Failure err ->
                p [] [ text (errorToString err) ]
        ]


errorToString : Error -> String
errorToString e =
    "Yikes. "
        ++ (case e of
                BadUrl str ->
                    "Invalid URL: " ++ str

                Timeout ->
                    "Request timed out. Make sure OpenDota is functioning"

                NetworkError ->
                    "Network error. Do you still have an active internet connection?"

                BadStatus status ->
                    "Bad HTTP status code - " ++ String.fromInt status ++ "."

                BadBody explanation ->
                    "Malformed JSON response. " ++ explanation
           )
