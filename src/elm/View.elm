module View exposing (view)

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
startView state =
    [ div [ class "flex flex-col text-center items-center" ]
        [ label [ class "input__label" ] [ text "Player 1 Name" ]
        , input [ class "input__text mb-3", type_ "text", value state.player1.name, ChangeName 1 |> onInput ] []
        , label [ class "input__label" ] [ text "Player 2 Name" ]
        , input [ class "input__text mb-3", type_ "text", value state.player2.name, ChangeName 2 |> onInput ] []
        , button
            [ disabled <| state.player1.name == "" || state.player2.name == ""
            , onClick StartGame
            , class "button"
            ]
            [ text "Start Game" ]
        ]
    ]


playingView : PlayingModel -> List (Html Msg)
playingView state =
    [ text "Playing" ]


endView : EndModel -> List (Html Msg)
endView state =
    [ text "End" ]
