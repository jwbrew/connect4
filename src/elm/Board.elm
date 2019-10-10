module Board exposing (Board, Col, MoveRequest(..), MoveResult(..), doMove, init)

import Array exposing (Array)
import Dict exposing (Dict)


type alias Board comparable =
    Dict Int (Col comparable)


type alias Col comparable =
    Array (Cell comparable)


type MoveRequest comparable
    = MoveRequest comparable Int


type MoveResult comparable
    = Valid (Board comparable)
    | Invalid String


type alias Cell comparable =
    Maybe comparable


init : Board comparable
init =
    List.range 0 6
        |> List.map (\x -> ( x, Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ] ))
        |> Dict.fromList


doMove : Board comparable -> MoveRequest comparable -> MoveResult comparable
doMove board (MoveRequest piece colIdx) =
    case Dict.get colIdx board of
        Just col ->
            case Array.toList col |> List.filterMap identity |> List.length of
                6 ->
                    Invalid "Column Full"

                n ->
                    let
                        newCol =
                            Array.set n (Just piece) col
                    in
                    Dict.insert colIdx newCol board |> Valid

        Nothing ->
            Invalid "Column Not Available"
