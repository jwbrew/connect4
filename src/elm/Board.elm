module Board exposing (Board, Col, MoveRequest(..), MoveResult(..), doMove, init)

import Array exposing (Array)
import Dict exposing (Dict)


type alias Board comparable =
    Array (Col comparable)


type alias Col comparable =
    Array (Cell comparable)


type MoveRequest comparable
    = MoveRequest comparable Int


type MoveResult comparable
    = Valid (Board comparable)
    | Invalid String
    | Draw (Board comparable)
    | Winner (Board comparable) comparable


type alias Cell comparable =
    Maybe comparable


init : Board comparable
init =
    List.range 0 6
        |> List.map (\_ -> Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ])
        |> Array.fromList


doMove : Board comparable -> MoveRequest comparable -> MoveResult comparable
doMove board (MoveRequest piece colIdx) =
    case Array.get colIdx board of
        Just col ->
            case Array.toList col |> List.filterMap identity |> List.length of
                6 ->
                    Invalid "Column Full"

                n ->
                    let
                        newCol =
                            Array.set n (Just piece) col
                    in
                    Array.set colIdx newCol board
                        |> checkResult

        Nothing ->
            Invalid "Column Not Available"


checkResult : Board comparable -> MoveResult comparable
checkResult board =
    Valid board
