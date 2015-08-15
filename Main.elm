import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Text exposing (..)
import Signal
import Window


main = 
  Signal.map2 view Window.dimensions boardClick.signal


type alias Row = Int
type alias Col = Int


type alias Position a = Maybe (a, a)


boardClick = Signal.mailbox Nothing


type Move = Empty
          | X
          | O


model = [[X, O, X], 
         [Empty, X, O],
         [X, O, Empty]]


view : (Int, Int) -> Position (number, number) -> Element
view (wx, wy) click = 
  container wx wy middle (flow down [viewClick click, viewBoard model])

viewClick click = 
  show click

viewBoard board =
  flow down (List.map2 viewRow [1, 2, 3] model)

viewRow : number -> List Move -> Element
viewRow rid moves =
  flow right (List.map2 (viewMove rid) [1..3] moves)


viewMove : number -> number -> Move -> Element
viewMove row col m = 
  case m of
    Empty -> 
      styleMove " "
        |> clickable (Signal.message boardClick.address (Just (row, col)))


    X ->
      styleMove "X"
        |> clickable (Signal.message boardClick.address (Just (row, col)))


    O -> 
      styleMove "O"
        |> clickable (Signal.message boardClick.address (Just (row, col)))


styleMove m =
  fromString m
    |> centered
    |> size 20 20
