module Lane exposing (..)

import Json.Decode exposing (Decoder, andThen, fail, field, int, map, succeed)


type alias LaneID =
    Int


type Lane
    = Safe
    | Mid
    | Off
    | Jungle
    | Roaming


toID : Lane -> Int
toID lane =
    case lane of
        Safe ->
            1

        Mid ->
            2

        Off ->
            3

        Jungle ->
            4

        Roaming ->
            5


fromID : Int -> Maybe Lane
fromID id =
    case id of
        1 ->
            Just Safe

        2 ->
            Just Mid

        3 ->
            Just Off

        4 ->
            Just Jungle

        5 ->
            Just Roaming

        _ ->
            Nothing


opposing : Lane -> List Lane
opposing lane =
    case lane of
        Safe ->
            [Off, Jungle, Roaming]

        Mid ->
            [Mid]

        Off ->
            [Safe, Roaming]

        -- these cases don't really make sense, but they don't appear in the
        -- lane dropdown anyway. yeah I know it's rounding corners, alright?
        Jungle ->
            [Safe, Roaming]

        Roaming ->
            [Safe, Mid, Off, Jungle, Roaming]


decoder : Decoder Lane
decoder =
    let
        decode id =
            case fromID id of
                Just laneID ->
                    succeed laneID

                Nothing ->
                    fail ("Lane ID " ++ String.fromInt id ++ " not between 1-5")
    in
    int |> andThen decode
