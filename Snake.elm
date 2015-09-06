module Snake (Model, Input, view, update, init, inputs) where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color
import Signal exposing (Address, message)
import Time
import Keyboard
import Debug

type alias Point = { x: Float, y: Float }
type alias Vector = { x: Float, y: Float }

type alias Model =
 { message : String
 , snake : List Point
 , direction : Vector
 }


type Input = Keyboard {x: Int, y: Int}
           | Tick

canvasSize = { width = 800, height = 600 }
topStatsSize = { width = 800, height = 20, x = 0, y = 290 }
boardSize = { width = 800, height = 580 }
snakeSize = { width = 10, height = 10 }


init : Model
init = { message = "snake!"
       , snake = [{ x = 0, y = 1}, { x = 0, y = 0}]
       , direction = { x = 0, y = 1}
       }


inputs = [ Signal.map (\tick -> Tick) (Time.fps 5)
         , Signal.map (\direction -> Keyboard direction) Keyboard.arrows ]


update : Input -> Model -> Model
update i m = 
  Debug.watch "Snake model" (updateModel i m)


updateModel i m =
  case i of 
    Keyboard d -> updateFromInput d m
    Tick -> moveSnake m


updateFromInput : {x: Int, y: Int} -> Model -> Model
updateFromInput d m =
  let
    direction = { x = toFloat d.x, y = toFloat d.y }
    dotProduct = m.direction.x * direction.x + m.direction.y * direction.y
  in
    if (d.x /= 0 || d.y /= 0) && dotProduct == 0 then
      { m | direction <- direction }
    else
      m


moveSnake : Model -> Model
moveSnake m =
  -- for now... need wrap around, wall death, etc.
  let
    firstPoint = Maybe.withDefault {x=0,y=0} (List.head m.snake)
    newPoint = moveSnakePoint m.direction firstPoint
  in
    { m | snake <- newPoint :: List.take (List.length m.snake - 1) m.snake }


moveSnakePoint d p = 
  { x = p.x + d.x, y = p.y + d.y}


view : Address Input -> Model -> Element
view a m =
  collage (round canvasSize.width) (round canvasSize.height) (forms m)


forms : Model -> List Form
forms m = [board,
           border,
           snake m.snake]

board : Form
board =
  rect canvasSize.width canvasSize.height
    |> filled Color.yellow


border : Form
border =
  let
    initialLineStyle = solid Color.orange
    style = { initialLineStyle | width <- 10 }
  in
    outlined style (rect canvasSize.width canvasSize.height)


snake : List Point -> Form
snake s =
  let
    collagePoints = List.map (\p -> { p | x <- p.x * 10, y <- p.y * 10}) s
    drawPoint p = move (p.x, p.y) (filled Color.red (rect 10 10))
  in
    group (List.map drawPoint collagePoints)
