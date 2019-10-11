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
        [ label [ class "input__label" ] [ text "Player 1" ]
        , div [ class "flex items-center mb-3" ]
            [ input
                [ class "input__text mr-3"
                , type_ "text"
                , value model.player1.name
                , ChangeName 1 |> onInput
                , placeholder "Name"
                ]
                []
            , a [ onClick <| ChangeColour 1, class "cursor-pointer" ] [ cellPiece model.player1 ]
            ]
        , label [ class "input__label" ] [ text "Player 2" ]
        , div [ class "flex items-center mb-3" ]
            [ input
                [ class "input__text mr-3"
                , type_ "text"
                , value model.player2.name
                , ChangeName 2 |> onInput
                , placeholder "Name"
                ]
                []
            , a [ onClick <| ChangeColour 2, class "cursor-pointer" ] [ cellPiece model.player2 ]
            ]
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
            ++ [ boardActions model
               , board model
               , if model.activePlayer == 1 then
                    span [ class "text-lg mt-3 font-semibold" ] [ text <| model.player1.name ++ " to move" ]

                 else
                    span [ class "text-lg mt-3 font-semibold" ]
                        [ text <| model.player2.name ++ " to move" ]
               , timers
                    model.player1
                    model.player2
               ]
    ]


boardActions : PlayingModel -> Html Msg
boardActions model =
    model.board
        |> Array.toList
        |> List.indexedMap
            (\idx _ ->
                div [ class "board__action" ]
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
    div [ class "board__cell" ]
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
    div [ class "timers" ]
        [ span [ class "timers__timer mb-2" ]
            [ cellPiece player1
            , span [ class "timers__name" ] [ text <| player1.name ++ ": " ]
            , span [ class "timers__clock" ] [ formatTime player1.playTime |> text ]
            , span [ class "timers__wincount" ] [ text <| "Win Count: " ++ String.fromInt player1.wins ]
            ]
        , span [ class "timers__timer" ]
            [ cellPiece player2
            , span [ class "timers__name" ] [ text <| player2.name ++ ": " ]
            , span [ class "timers__clock" ] [ formatTime player2.playTime |> text ]
            , span [ class "timers__wincount" ] [ text <| "Win Count: " ++ String.fromInt player2.wins ]
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

        Pink ->
            div [ class "piece piece--pink" ] []

        Yellow ->
            div [ class "piece piece--yellow" ] []

        Purple ->
            div [ class "piece piece--purple" ] []

        Green ->
            div [ class "piece piece--green" ] []


endView : EndModel -> List (Html Msg)
endView model =
    [ div [ class "end" ]
        [ case model.winner of
            Just 1 ->
                div [ class "flex flex-col items-center mb-3" ]
                    [ cellPiece model.player1
                    , span [ class "end__title" ] [ text <| model.player1.name ++ " Wins!" ]
                    ]

            Just 2 ->
                div [ class "flex flex-col items-center mb-3" ]
                    [ cellPiece model.player2
                    , span [ class "end__title" ] [ text <| model.player2.name ++ " Wins!" ]
                    ]

            Nothing ->
                span [ class "end__title" ] [ text "It's a Draw!" ]

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
