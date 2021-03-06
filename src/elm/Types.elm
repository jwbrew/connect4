module Types exposing (..)

import Board
import Time


type Colour
    = Blue
    | Red
    | Pink
    | Purple
    | Green
    | Yellow


type alias ID =
    Int


type alias Board =
    Board.Board ID


type alias Col =
    Board.Col ID


type alias MoveRequest =
    Board.MoveRequest ID


type alias Player =
    { id : ID, name : String, playTime : Int, colour : Colour, wins : Int }


type Model
    = Start StartModel
    | Playing PlayingModel
    | End EndModel


type alias StartModel =
    { player1 : Player
    , player2 : Player
    }


type alias PlayingModel =
    { player1 : Player
    , player2 : Player
    , board : Board
    , error : Maybe String
    , activePlayer : ID
    }


type alias EndModel =
    { player1 : Player
    , player2 : Player
    , winner : Maybe ID
    , board : Board
    }


type alias Flags =
    ()


type Msg
    = ChangeName ID String
    | ChangeColour ID
    | SwitchColour
    | StartGame
    | AttemptMove MoveRequest
    | Restart
    | Reset
    | Tick Time.Posix
    | NoOp
