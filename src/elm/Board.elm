module Board exposing (Board, Col, MoveRequest(..), MoveResult(..), doMove, init)

import Array exposing (Array)
import Dict exposing (Dict)
import Set


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


type LineStatus comparable
    = Clear
    | Checking comparable Int
    | Won comparable


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
    let
        rangeX =
            List.range 0 6

        rangeY =
            List.range 0 5

        vertical =
            rangeX
                |> List.map (\x -> rangeY |> List.map (\y -> ( x, y )))

        horizontal =
            rangeY
                |> List.map (\y -> rangeX |> List.map (\x -> ( x, y )))

        -- BL to TR - x increases, y increases
        -- Start on LHS, clamp Y between given and 5
        diagonal_a =
            List.map
                (\y ->
                    List.range y 5
                        |> List.indexedMap (\idx x -> ( idx, x ))
                )
                rangeY

        -- Continue across bottom, clamp X between given and 6
        diagonal_b =
            List.map
                (\x ->
                    List.range x 6
                        |> List.indexedMap (\idx y -> ( y, idx ))
                )
                rangeX

        -- TL to BR - x increases, y decreases
        -- Start on LHS, clamp X between given and 0
        diagonal_c =
            List.map
                (\y ->
                    List.range 0 y
                        |> List.reverse
                        |> List.indexedMap (\idx x -> ( idx, x ))
                )
                rangeY

        -- Continue across top, clamp X between given and 6
        diagonal_d =
            List.map
                (\x ->
                    List.range 0 x
                        |> List.reverse
                        |> List.indexedMap (\idx y -> ( y, idx ))
                )
                rangeX

        lines =
            vertical
                ++ horizontal
                ++ diagonal_a
                ++ diagonal_b
                ++ diagonal_c
                ++ diagonal_d
                -- Trim edges
                |> List.map (List.filter (\( x, y ) -> x <= 6 && y <= 5))
                -- Trim Grab moves from board
                |> List.map (List.map (\( x, y ) -> Array.get x board |> Maybe.andThen (\col -> Array.get y col) |> Maybe.andThen identity))
                -- Check for a winner
                |> List.map (List.foldl checkLine Clear)
                |> List.filterMap
                    (\x ->
                        case x of
                            Won id ->
                                Just id

                            _ ->
                                Nothing
                    )
                -- Unique list
                |> Set.fromList
                |> Set.toList
    in
    if
        board
            |> Array.map Array.toList
            |> Array.toList
            |> List.concat
            |> List.filterMap identity
            |> List.length
            |> (==) (6 * 7)
    then
        Draw board

    else
        case lines of
            [ x ] ->
                Winner board x

            _ ->
                Valid board


checkLine : Cell comparable -> LineStatus comparable -> LineStatus comparable
checkLine cell status =
    case ( cell, status ) of
        ( _, Won comparable ) ->
            Won comparable

        ( Just id, Clear ) ->
            Checking id 1

        ( Just id, Checking lastID 3 ) ->
            if id == lastID then
                Won id

            else
                Checking id 1

        ( Just id, Checking lastID count ) ->
            if id == lastID then
                Checking id <| count + 1

            else
                Checking id 1

        ( Nothing, _ ) ->
            Clear
