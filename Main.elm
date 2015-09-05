import Signal
import Window
import Tictactoe
import Snake
import Graphics.Element exposing (..)
import Graphics.Input exposing (clickable)
import Text exposing (fromString)

-- Infrastructure, see also StartApp
main : Signal Element
main = 
  Signal.map2 view Window.dimensions currentState

currentState : Signal Model
currentState = Signal.foldp
               (\(Just action) model -> update action model)
               init
               inputs

mailbox : Signal.Mailbox (Maybe Action)
mailbox = Signal.mailbox Nothing

address = Signal.forwardTo mailbox.address Just

inputs = Signal.mergeMany (mailbox.signal :: List.map (Signal.map (\x -> Just (SnakeMove x))) Snake.inputs)

-- Main App itself
type Action = MenuSelection Toy
            | TictactoeMove Tictactoe.Action
            | SnakeMove Snake.Input

t3Address = Signal.forwardTo address TictactoeMove
snakeAddress = Signal.forwardTo address SnakeMove
menuAddress = Signal.forwardTo address MenuSelection

-- Model
type Toy = Menu | TictactoeGame | SnakeGame

type alias Model = { currentToy: Toy, t3m : Tictactoe.Model, s : Snake.Model }
init : Model
init = { currentToy = Menu, t3m = Tictactoe.init, s = Snake.init }


-- Update
update : Action -> Model -> Model
update a m =
  case a of
    MenuSelection t -> { m | currentToy <- t }
    TictactoeMove ta -> { m | t3m <- Tictactoe.update ta m.t3m }
    SnakeMove i -> { m | s <- Snake.update i m.s }


-- View
view : (Int, Int) -> Model -> Element
view w m = 
  case m.currentToy of
    Menu -> viewMenu menuAddress w m
    TictactoeGame -> (Tictactoe.view t3Address w m.t3m) `below` (viewMainMenuButton menuAddress)
    SnakeGame -> (Snake.view snakeAddress m.s) `below` (viewMainMenuButton menuAddress)


viewMainMenuButton : (Signal.Address Toy) -> Element
viewMainMenuButton a =
  fromString "Menu Menu"
    |> centered
    |> clickable (Signal.message a Menu)


viewMenu : (Signal.Address Toy) -> (Int, Int) -> Model -> Element
viewMenu a (wx, wy) m =
  container wx wy middle (flow down [viewMenuItem a "Tictactoe" TictactoeGame,
                                     viewMenuItem a "Snake" SnakeGame])

viewMenuItem : Signal.Address Toy -> String -> Toy -> Element
viewMenuItem a s t =
  fromString s
    |> centered
    |> clickable (Signal.message a t)
