import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Text exposing (..)
import Signal
import Window
import Debug exposing (..)
import Array

main = 
  Signal.map2 view Window.dimensions currentState

currentState = Debug.watch "state" (Signal.foldp update init boardClick.signal)



type alias Row = Int
type alias Col = Int


boardClick = Signal.mailbox Nothing


type Player = X | O


type Piece = Empty
          | PX
          | PO


type Action = PlayMove Int Int

type alias Model = { board : Array.Array Piece,
                     lastClick : Maybe Action,
                     nextPlayer : Player }

init = { board = Array.repeat 9 Empty, 
         lastClick = Nothing,
         nextPlayer = X }

update : Maybe Action -> Model -> Model
update a m = 
  case a of
    Just (PlayMove x y) -> { m | nextPlayer <- otherPlayer m.nextPlayer, 
                             board <- Array.set (y + 3 * x) (pieceForPlayer m.nextPlayer) m.board,
                             lastClick <- a }

-- Was it dumb to separate types of pieces and players? 
pieceForPlayer : Player -> Piece
pieceForPlayer p = 
  case p of 
    X -> PX
    O -> PO


otherPlayer : Player -> Player
otherPlayer p =
  case p of 
    X -> O
    O -> X

view : (Int, Int) -> Model -> Element
view (wx, wy) model = 
  container wx wy middle (flow down [viewClick model.lastClick, 
                                     viewNextPlayer model.nextPlayer,
                                     viewBoard model.board])

viewNextPlayer np = 
  show np

viewClick click = 
  show click

viewBoard board =
  flow down (List.map2 viewRow [0..2] [Array.toList (Array.slice 0 3 board), 
                                       Array.toList (Array.slice 3 6 board), 
                                       Array.toList (Array.slice 6 9 board)])

viewRow : number -> List Piece -> Element
viewRow rid moves =
  flow right (List.map2 (viewPiece rid) [0..2] moves)


viewPiece : number -> number -> Piece -> Element
viewPiece row col p = 
  case p of
    Empty -> 
      stylePiece "_"
        |> clickable (Signal.message boardClick.address (Just (PlayMove row col)))


    PX ->
      stylePiece "X"
        |> clickable (Signal.message boardClick.address (Just (PlayMove row col)))


    PO -> 
      stylePiece "O"
        |> clickable (Signal.message boardClick.address (Just (PlayMove row col)))


stylePiece p =
  fromString p
    |> centered
    |> size 20 20
