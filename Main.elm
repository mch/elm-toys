import Signal
import Window
import Tictactoe
import Graphics.Element exposing (..)

main : Signal Element
main = 
  Signal.map2 view Window.dimensions currentState

currentState : Signal Tictactoe.Model
currentState = Signal.foldp
               (\(Just action) model -> update action model)
               init
               boardClick.signal

boardClick : Signal.Mailbox (Maybe Tictactoe.Action)
boardClick = Signal.mailbox Nothing

address = Signal.forwardTo boardClick.address Just

-- Model
type alias Model = Tictactoe.Model
init : Tictactoe.Model
init = Tictactoe.init

-- Update
update : Tictactoe.Action -> Model -> Model
update = Tictactoe.update

-- View
view = Tictactoe.view address
