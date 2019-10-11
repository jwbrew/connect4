module View exposing (view)

import Array
import Board
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time
import Types exposing (..)


view : Model -> List (Html Msg)
view model =
    [ div [ class "w-screen h-screen bg-gray-100 flex items-center justify-center" ] <|
        case model of
            Start state ->
                startView state

            Playing state ->
                playingView state

            End state ->
                endView state
    ]


startView : StartModel -> List (Html Msg)
startView model =
    [ div [ class "flex flex-col text-center items-center" ]
        [ label [ class "input__label" ] [ text "Player 1 Name" ]
        , input [ class "input__text mb-3", type_ "text", value model.player1.name, ChangeName 1 |> onInput ] []
        , label [ class "input__label" ] [ text "Player 2 Name" ]
        , input [ class "input__text mb-3", type_ "text", value model.player2.name, ChangeName 2 |> onInput ] []
        , button
            [ disabled <| model.player1.name == "" || model.player2.name == ""
            , onClick StartGame
            , class "button"
            ]
            [ text "Start Game" ]
        ]
    ]


playingView : PlayingModel -> List (Html Msg)
playingView model =
    [ div [ class "flex flex-col items-center" ] <|
        [ case model.error of
            Just string ->
                div [ class "alert--error mb-3" ] [ text string ]

            Nothing ->
                text ""
        ]
            ++ [ boardActions model, board model, timers model.player1 model.player2 ]
    ]


boardActions : PlayingModel -> Html Msg
boardActions model =
    model.board
        |> Array.toList
        |> List.indexedMap
            (\idx _ ->
                div [ class "w-16 h-16 flex justify-center items-center" ]
                    [ button
                        [ class "p-4", Board.MoveRequest model.activePlayer idx |> AttemptMove |> onClick ]
                        [ text "↓" ]
                    ]
            )
        |> div [ class "flex" ]


board : { a | board : Board, player1 : Player, player2 : Player } -> Html Msg
board model =
    model.board
        |> Array.toList
        |> List.indexedMap (boardColumn model)
        |> div [ class "flex" ]


boardColumn : { a | board : Board, player1 : Player, player2 : Player } -> Int -> Col -> Html Msg
boardColumn model idx col =
    let
        cells =
            col |> Array.toList |> List.map (boardCell model)
    in
    div [ class "flex flex-col-reverse" ] cells


boardCell : { a | board : Board, player1 : Player, player2 : Player } -> Maybe ID -> Html Msg
boardCell model maybeId =
    div [ class "w-16 h-16 border flex justify-center items-center" ]
        [ case maybeId of
            Just 1 ->
                cellPiece model.player1

            Just 2 ->
                cellPiece model.player2

            Nothing ->
                span [] []

            _ ->
                text ""
        ]


timers : Player -> Player -> Html Msg
timers player1 player2 =
    div [ class "flex flex-col mt-6" ]
        [ span [ class "flex items-center mb-2" ]
            [ cellPiece player1
            , span [ class "text-gray-600 text-sm uppercase ml-4" ] [ text <| player1.name ++ ": " ]
            , span [ class "text-lg w-48 block ml-2" ] [ formatTime player1.playTime |> text ]
            ]
        , span [ class "flex items-center" ]
            [ cellPiece player2
            , span [ class "text-gray-600 text-sm uppercase ml-4" ] [ text <| player2.name ++ ": " ]
            , span [ class "text-lg w-48 block ml-2" ] [ formatTime player2.playTime |> text ]
            ]
        ]


formatTime : Int -> String
formatTime int =
    let
        time =
            Time.millisToPosix int
    in
    String.fromInt (Time.toHour Time.utc time)
        ++ ":"
        ++ String.fromInt (Time.toMinute Time.utc time)
        ++ ":"
        ++ String.fromInt (Time.toSecond Time.utc time)
        ++ ":"
        ++ String.fromInt (Time.toMillis Time.utc time)


cellPiece : Player -> Html Msg
cellPiece player =
    case player.colour of
        Blue ->
            div [ class "piece piece--blue" ] []

        Red ->
            div [ class "piece piece--red" ] []


endView : EndModel -> List (Html Msg)
endView model =
    [ div [ class "flex flex-col items-center" ]
        [ case model.winner of
            Just 1 ->
                div [ class "flex flex-col items-center mb-3" ]
                    [ cellPiece model.player1
                    , span [ class "text-2xl my-6" ] [ text <| model.player1.name ++ " Wins!" ]
                    ]

            Just 2 ->
                div [ class "flex flex-col items-center mb-3" ]
                    [ cellPiece model.player2
                    , span [ class "text-2xl my-6" ] [ text <| model.player2.name ++ " Wins!" ]
                    ]

            Nothing ->
                span [ class "text-2xl mb-3" ] [ text "It's a Draw!" ]

            _ ->
                text ""
        , board model
        , timers model.player1 model.player2
        , div [ class "flex mt-6" ]
            [ button [ onClick Reset, class "button mr-2" ] [ text "Reset" ]
            , button [ onClick Restart, class "button" ] [ text "Restart" ]
            ]
        ]
    ]
