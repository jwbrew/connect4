module Main exposing (main)

import Board
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Time
import Url exposing (Url)


type Colour
    = Blue
    | Red


type alias ID =
    Int


type alias Board =
    Board.Board ID


type alias Player =
    { id : ID, name : String, playTime : Int, colour : Colour }


type Model
    = Start
        { player1 : Player
        , player2 : Player
        }
    | Playing
        { player1 : Player
        , player2 : Player
        , board : Board
        , error : Maybe String
        , activePlayer : ID
        }
    | End
        { player1 : Player
        , player2 : Player
        , winner : Player
        , board : Board
        }


type alias Flags =
    ()


type Msg
    = ChangeName ID String
    | SwitchColour
    | StartGame
    | AttemptMove (Board.MoveRequest ID)
    | Restart
    | Reset
    | Tick Time.Posix
    | NoOp


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
                    ( Playing { state | player1 = (\p -> { p | playTime = p.playTime + interval }) state.player1 }, Cmd.none )

                2 ->
                    ( Playing { state | player2 = (\p -> { p | playTime = p.playTime + interval }) state.player2 }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( AttemptMove request, Playing state ) ->
            case Board.doMove state.board request of
                Board.Valid board ->
                    ( Playing { state | board = board, error = Nothing }, Cmd.none )

                Board.Invalid reason ->
                    ( Playing { state | error = Just reason }, Cmd.none )

        ( Restart, End state ) ->
            ( model, Cmd.none )

        ( Reset, End state ) ->
            ( model, Cmd.none )

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
            \_ ->
                { title = "Connect 4"
                , body = []
                }
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }
