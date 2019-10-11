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
        { player1 = Player 1 "" 0 Blue 0
        , player2 = Player 2 "" 0 Red 0
        }
    , Cmd.none
    )


interval =
    1


cycleColour : Player -> Player -> Player
cycleColour player otherPlayer =
    let
        newPlayer =
            { player
                | colour =
                    case player.colour of
                        Blue ->
                            Red

                        Red ->
                            Pink

                        Pink ->
                            Purple

                        Purple ->
                            Green

                        Green ->
                            Yellow

                        Yellow ->
                            Blue
            }
    in
    if newPlayer.colour == otherPlayer.colour then
        cycleColour newPlayer otherPlayer

    else
        newPlayer


updatePlayer :
    (Player -> Player)
    -> ID
    -> { a | player1 : Player, player2 : Player }
    -> { a | player1 : Player, player2 : Player }
updatePlayer function id model =
    case id of
        1 ->
            { model | player1 = function model.player1 }

        2 ->
            { model | player2 = function model.player2 }

        _ ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ChangeName id name, Start players ) ->
            ( updatePlayer (\p -> { p | name = name }) id players |> Start, Cmd.none )

        ( ChangeColour id, Start players ) ->
            case id of
                1 ->
                    ( Start { players | player1 = cycleColour players.player1 players.player2 }, Cmd.none )

                2 ->
                    ( Start { players | player2 = cycleColour players.player2 players.player1 }, Cmd.none )

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
            ( updatePlayer (\p -> { p | playTime = p.playTime + interval }) state.activePlayer state |> Playing, Cmd.none )

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
                    ( { player1 = state.player1
                      , player2 = state.player2
                      , winner = Just winner
                      , board = board
                      }
                        |> updatePlayer (\p -> { p | wins = p.wins + 1 }) winner
                        |> End
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
                { player1 = Player 1 "" 0 Blue 0
                , player2 = Player 2 "" 0 Red 0
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
