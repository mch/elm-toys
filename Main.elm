import Signal
import Window
import Tictactoe
import Graphics.Element exposing (..)

main : Signal Element
main = 
  Signal.map2 view Window.dimensions currentState

currentState : Signal Tictactoe.Model
currentState = Signal.foldp update init boardClick.signal

boardClick : Signal.Mailbox (Maybe Tictactoe.Action)
boardClick = Signal.mailbox Nothing

-- Model
type alias Model = Tictactoe.Model
init : Tictactoe.Model
init = Tictactoe.init

-- Update
update : Maybe Tictactoe.Action -> Model -> Model
update = Tictactoe.update

-- View
view = Tictactoe.view boardClick.address
