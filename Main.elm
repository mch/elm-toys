import Signal
import Window
import Tictactoe
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
type Action = Move Tictactoe.Action

moveAddress = Signal.forwardTo address Move

-- Model
type alias Model = { t3m : Tictactoe.Model }
init : Model
init = { t3m = Tictactoe.init }


-- Update
update : Action -> Model -> Model
update a m =
  case a of
    Move ta -> { m | t3m <- Tictactoe.update ta m.t3m }


-- View
view : (Int, Int) -> Model -> Element
view w m = Tictactoe.view moveAddress w m.t3m
