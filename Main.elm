import Signal
import Window
import Tictactoe
import Graphics.Element exposing (..)

main : Signal Element
main = 
  Signal.map2 (Tictactoe.view boardClick.address) Window.dimensions currentState


currentState : Signal Tictactoe.Model
currentState = Signal.foldp Tictactoe.update Tictactoe.init boardClick.signal

-- Should this level need to know about the Action type? 
--boardClick : Signal.Mailbox (Maybe Action)
boardClick = Signal.mailbox Nothing


