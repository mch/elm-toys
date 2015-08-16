import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Text exposing (..)
import Signal
import Window
import Debug exposing (..)
import Array
import Random

main = 
  Signal.map2 view Window.dimensions currentState


currentState = Signal.foldp update init boardClick.signal


type alias Row = Int
type alias Col = Int


boardClick = Signal.mailbox Nothing


-- Player one, Player two
type Player = P1 | P2


type Piece = Empty
          | X
          | O


type Action = PlayMove Int Int


type alias Model = { board : Array.Array Piece,
                     lastClick : Maybe Action,
                     nextPlayer : Player,
                     seed : Random.Seed, 
                     message : String, 
                     winner : Maybe Player }


init = { board = Array.repeat 9 Empty, 
         lastClick = Nothing,
         nextPlayer = P1, 
         seed = Random.initialSeed 1,
         message = "",
         winner = Nothing }
{- 
need current time for the seed... best for a seed seems to be to use
JavaScript interop, but then need to embed Elm and it would not work
well with elm-reactor etc.
-} 


update : Maybe Action -> Model -> Model
update a m = 
  case a of
    Just (PlayMove x y) -> 
      let 
        (gameOver, winner) = isGameOver m.board
      in
        if | (isMoveValid m.board x y) -> (applyMoveAndCheckWinner a m x y)
           | otherwise -> ridiculePlayer m

    Nothing -> m


applyMoveAndCheckWinner a m x y =
  let 
    newState = applyMove a m x y
    (gameOver, winner) = isGameOver newState.board
  in
    if gameOver then { newState | message <- "Game over" } else newState


isMoveValid : Array.Array Piece -> Int -> Int -> Bool
isMoveValid board x y = Array.get (y + 3 * x) board == Just Empty
  

lines = [[0, 1, 2],
         [3, 4, 5],
         [6, 7, 8],
         [0, 3, 6],
         [1, 4, 7],
         [2, 5, 8],
         [0, 4, 8],
         [2, 4, 6]]

isGameOver : Array.Array Piece -> (Bool, Maybe Player)
isGameOver board = 
  let 
    complete = Array.isEmpty (Array.filter (\x -> x == Empty) board)
    boardLines = List.map (\y -> (List.map (\x -> Maybe.withDefault Empty (Array.get x board)) y)) lines
    xLines = Debug.watch "all xes" (List.map (List.foldl (\x y -> y && (x == X)) True) boardLines)
    oLines = Debug.watch "all Oes" (List.map (List.foldl (\x y -> y && (x == O)) True) boardLines)
  in
    (complete, Nothing)



applyMove a m x y = 
  { m | nextPlayer <- otherPlayer m.nextPlayer, 
    board <- Array.set (y + 3 * x) (pieceForPlayer m.nextPlayer) m.board,
    lastClick <- a,
    message <- "" }


defaultMessage = "Sorry, that is not a valid move"
messages = 
  Array.fromList [defaultMessage,
                  "Seriously, come on.",
                  "You can't play there!",
                  "Where did you learn to play Tic Tac Toe?"]


ridiculePlayer m = 
  let numMessages = Array.length messages
      (i, newseed) = Random.generate (Random.int 0 numMessages) m.seed
      message = Maybe.withDefault defaultMessage (Array.get i messages)
  in
    { m | message <- message, seed <- newseed }


-- Was it dumb to separate types of pieces and players? 
pieceForPlayer : Player -> Piece
pieceForPlayer p = 
  case p of 
    P1 -> X
    P2 -> O


otherPlayer : Player -> Player
otherPlayer p =
  case p of 
    P1 -> P2
    P2 -> P1


view : (Int, Int) -> Model -> Element
view (wx, wy) model = 
  container wx wy middle (flow down [viewMessage model.message, 
                                     viewNextPlayer model.nextPlayer,
                                     viewBoard model.board])

viewNextPlayer np = 
  "Next player: " ++ toString np
    |> fromString
    |> centered
    |> size 160 40


viewMessage message = 
  message
    |> fromString 
    |> centered
    |> size 160 40


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


    X ->
      stylePiece "X"
        |> clickable (Signal.message boardClick.address (Just (PlayMove row col)))


    O -> 
      stylePiece "O"
        |> clickable (Signal.message boardClick.address (Just (PlayMove row col)))


stylePiece p =
  fromString p
    |> centered
    |> size 40 40
