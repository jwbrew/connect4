module Board exposing (Board, MoveRequest, MoveResult(..), doMove, init)

import Dict exposing (Dict)


type Board comparable
    = Board (Dict Int (Col comparable))


type alias Col comparable =
    List comparable


type MoveRequest comparable
    = MoveRequest Int comparable


type MoveResult comparable
    = Valid (Board comparable)
    | Invalid String


init : Board comparable
init =
    List.range 0 6
        |> List.map (\x -> ( x, [] ))
        |> Dict.fromList
        |> Board


doMove : Board comparable -> MoveRequest comparable -> MoveResult comparable
doMove (Board board) (MoveRequest col piece) =
    case Dict.get col board of
        Just [ a, b, c, d, e, f ] ->
            Invalid "Column Full"

        Just xs ->
            Dict.insert col (piece :: xs) board
                |> Board
                |> Valid

        Nothing ->
            Invalid "Column Not Available"
