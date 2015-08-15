import Graphics.Element exposing (..)

main = view

type Move = Empty
          | X
          | O

model = [[X, O, X], 
         [Empty, X, O],
         [X, O, Empty]]


view = 
  (flow down (List.map viewRow model))

viewRow r = 
  flow right (List.map viewMove r)

viewMove : Move -> Element
viewMove m = 
  case m of
    Empty -> 
      show " "


    X ->
      show "X"


    O -> 
      show "O"
