import Signal
import Window
import Tictactoe
import Snake
import Graphics.Element exposing (..)


-- Infrastructure, see also StartApp
main : Signal Element
main = 
  Signal.map2 view Window.dimensions currentState

currentState : Signal Model
currentState = Signal.foldp
               (\(Just action) model -> update action model)
               init
               mailbox.signal

mailbox : Signal.Mailbox (Maybe Action)
mailbox = Signal.mailbox Nothing

address = Signal.forwardTo mailbox.address Just


-- Main App itself
type Action = TictactoeMove Tictactoe.Action
            | SnakeMove Snake.Input

t3Address = Signal.forwardTo address TictactoeMove
snakeAddress = Signal.forwardTo address SnakeMove

-- Model
type Game = TictactoeGame | SnakeGame

type alias Model = { currentGame: Game, t3m : Tictactoe.Model, s : Snake.Model }
init : Model
init = { currentGame = TictactoeGame, t3m = Tictactoe.init, s = Snake.init }


-- Update
update : Action -> Model -> Model
update a m =
  case a of
    TictactoeMove ta -> { m | t3m <- Tictactoe.update ta m.t3m }
    SnakeMove i -> { m | s <- Snake.update i m.s }


-- View
view : (Int, Int) -> Model -> Element
view w m = 
  case m.currentGame of
    TictactoeGame -> Tictactoe.view t3Address w m.t3m
    SnakeGame -> Snake.view snakeAddress m.s

