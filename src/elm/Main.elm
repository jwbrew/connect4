module Main exposing (main)

import Board
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Time
import Types exposing (..)
import Url exposing (Url)
import View


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    ( Start
        { player1 = Player 1 "" 0 Blue
        , player2 = Player 2 "" 0 Red
        }
    , Cmd.none
    )


interval =
    1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ChangeName id name, Start players ) ->
            case id of
                1 ->
                    ( Start { players | player1 = (\p -> { p | name = name }) players.player1 }, Cmd.none )

                2 ->
                    ( Start { players | player2 = (\p -> { p | name = name }) players.player2 }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( SwitchColour, Start players ) ->
            ( Start
                { players
                    | player1 = (\p -> { p | colour = players.player2.colour }) players.player1
                    , player2 = (\p -> { p | colour = players.player1.colour }) players.player2
                }
            , Cmd.none
            )

        ( StartGame, Start players ) ->
            ( Playing
                { player1 = players.player1
                , player2 = players.player2
                , board = Board.init
                , error = Nothing
                , activePlayer = 1
                }
            , Cmd.none
            )

        ( Tick _, Playing state ) ->
            case state.activePlayer of
                1 ->
                    ( Playing
                        { state
                            | player1 =
                                (\p -> { p | playTime = p.playTime + interval }) state.player1
                        }
                    , Cmd.none
                    )

                2 ->
                    ( Playing
                        { state
                            | player2 =
                                (\p -> { p | playTime = p.playTime + interval }) state.player2
                        }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ( AttemptMove request, Playing state ) ->
            case Board.doMove state.board request of
                Board.Valid board ->
                    ( Playing
                        { state
                            | board = board
                            , error = Nothing
                            , activePlayer =
                                if state.activePlayer == 1 then
                                    2

                                else
                                    1
                        }
                    , Cmd.none
                    )

                Board.Invalid reason ->
                    ( Playing { state | error = Just reason }, Cmd.none )

                Board.Draw board ->
                    ( End
                        { player1 = state.player1
                        , player2 = state.player2
                        , winner = Nothing
                        , board = board
                        }
                    , Cmd.none
                    )

                Board.Winner board winner ->
                    ( End
                        { player1 = state.player1
                        , player2 = state.player2
                        , winner = Just winner
                        , board = board
                        }
                    , Cmd.none
                    )

        ( Restart, End state ) ->
            ( Playing
                { player1 = state.player1
                , player2 = state.player2
                , board = Board.init
                , activePlayer = 1
                , error = Nothing
                }
            , Cmd.none
            )

        ( Reset, End state ) ->
            ( Start
                { player1 = Player 1 "" 0 Blue
                , player2 = Player 2 "" 0 Red
                }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every interval Tick


onUrlRequest : UrlRequest -> Msg
onUrlRequest urlRequest =
    NoOp


onUrlChange : Url -> Msg
onUrlChange url =
    NoOp


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view =
            \model ->
                { title = "Connect 4"
                , body = View.view model
                }
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }
