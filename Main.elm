import Signal
import Window
import Tictactoe
import Graphics.Element exposing (..)

main : Signal Element
main = 
  Signal.map2 view Window.dimensions currentState

currentState : Signal Model
currentState = Signal.foldp
               (\(Just action) model -> update action model)
               init
               boardClick.signal

boardClick : Signal.Mailbox (Maybe Tictactoe.Action)
boardClick = Signal.mailbox Nothing

address = Signal.forwardTo boardClick.address Just

-- Model
type alias Model = { t3m : Tictactoe.Model }
init : Model
init = { t3m = Tictactoe.init }

-- Update
update : Tictactoe.Action -> Model -> Model
update a m = { m | t3m <- Tictactoe.update a m.t3m }

-- View
view : (Int, Int) -> Model -> Element
view w m = Tictactoe.view address w m.t3m
