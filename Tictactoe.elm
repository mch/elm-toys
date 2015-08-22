module Tictactoe (Model, Action, view, update, init) where

import Signal exposing (Address, message)
import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Text exposing (..)
import Array exposing (Array)
import Random

type alias Row = Int
type alias Col = Int


-- Player one, Player two
type Player = P1 | P2


type Piece = Empty
          | X
          | O


type Action = PlayMove Int Int

type alias Board = Array.Array Piece

type alias Model = { board : Board,
                     lastClick : Maybe Action,
                     nextPlayer : Player,
                     seed : Random.Seed, 
                     message : String, 
                     winner : Maybe Player }


init : Model
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


update : Action -> Model -> Model
update a m = 
  case a of
    PlayMove x y -> 
      if | (isMoveValid m.board x y) -> (applyMoveAndCheckWinner a m x y)
         | otherwise -> ridiculePlayer m


applyMoveAndCheckWinner : Action -> Model -> number -> number -> Model
applyMoveAndCheckWinner a m x y =
  let 
    newState = applyMove a m x y
    gameOver = isGameOver newState.board m.nextPlayer
  in
    if gameOver then { newState | message <- "Game over" } else newState


isMoveValid : Board -> Int -> Int -> Bool
isMoveValid board x y = Array.get (y + 3 * x) board == Just Empty
  

lines : List (List Int)
lines = [[0, 1, 2],
         [3, 4, 5],
         [6, 7, 8],
         [0, 3, 6],
         [1, 4, 7],
         [2, 5, 8],
         [0, 4, 8],
         [2, 4, 6]]

isGameOver : Board -> Player -> Bool
isGameOver board player =
  let 
    complete = Array.isEmpty (Array.filter (\x -> x == Empty) board)
    boardLines = List.map (\y -> (List.map (\x -> Maybe.withDefault Empty (Array.get x board)) y)) lines
    piece = pieceForPlayer player
    playerLines = List.map (List.all (\x -> x == piece)) boardLines
    playerWins = List.any (\x -> x == True) playerLines
  in
    playerWins


applyMove : Action -> Model -> number -> number -> Model
applyMove a m x y = 
  { m | nextPlayer <- otherPlayer m.nextPlayer, 
    board <- Array.set (y + 3 * x) (pieceForPlayer m.nextPlayer) m.board,
    lastClick <- Just a,
    message <- "" }


defaultMessage : String
defaultMessage = "Sorry, that is not a valid move"


messages : Array String
messages = 
  Array.fromList [defaultMessage,
                  "Seriously, come on.",
                  "You can't play there!",
                  "Where did you learn to play Tic Tac Toe?"]


ridiculePlayer : Model -> Model
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


view : Address Action -> (Int, Int) -> Model -> Element
view address (wx, wy) model = 
  container wx wy middle (flow down [viewMessage model.message, 
                                     viewNextPlayer model.nextPlayer ,
                                     viewBoard address model.board])


viewNextPlayer : Player -> Element
viewNextPlayer np = 
  "Next player: " ++ toString np
    |> fromString
    |> centered
    |> size 160 40


viewMessage : String -> Element
viewMessage message = 
  message
    |> fromString 
    |> centered
    |> size 160 40

       
viewBoard : Address Action -> Board -> Element
viewBoard address board =
  flow down (List.map2 (viewRow address) [0..2] [Array.toList (Array.slice 0 3 board), 
                                                 Array.toList (Array.slice 3 6 board), 
                                                 Array.toList (Array.slice 6 9 board)])


viewRow : Address Action -> number -> List Piece -> Element
viewRow address rid moves =
  flow right (List.map2 (viewPiece address rid) [0..2] moves)


viewPiece : Address Action -> number -> number -> Piece -> Element
viewPiece address row col p = 
  case p of
    Empty -> 
      stylePiece "_"
        |> clickable (message address (PlayMove row col))


    X ->
      stylePiece "X"
        |> clickable (message address (PlayMove row col))


    O -> 
      stylePiece "O"
        |> clickable (message address (PlayMove row col))


stylePiece : String -> Element
stylePiece p =
  fromString p
    |> centered
    |> size 40 40
