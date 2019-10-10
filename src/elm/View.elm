module View exposing (view)

import Array
import Board
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
    let
        board =
            Dict.toList model.board |> List.map (boardColumn model) |> div [ class "flex" ]
    in
    [ div [ class "flex flex-col" ] <|
        [ case model.error of
            Just string ->
                div [ class "alert--error mb-3" ] [ text string ]

            Nothing ->
                text ""
        ]
            ++ [ board ]
    ]


boardColumn : PlayingModel -> ( Int, Col ) -> Html Msg
boardColumn model ( idx, col ) =
    let
        cells =
            col |> Array.toList |> List.map (boardCell model)

        action =
            button [ Board.MoveRequest model.activePlayer idx |> AttemptMove |> onClick ] [ text "↓" ]
    in
    div [ class "flex flex-col-reverse" ] (cells ++ [ action ])


boardCell : PlayingModel -> Maybe ID -> Html Msg
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


cellPiece : Player -> Html Msg
cellPiece player =
    case player.colour of
        Blue ->
            div [ class "piece piece--blue" ] []

        Red ->
            div [ class "piece piece--red" ] []


endView : EndModel -> List (Html Msg)
endView model =
    [ text "End" ]
