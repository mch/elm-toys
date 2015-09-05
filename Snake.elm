module Snake (Model, Input, view, update, init, inputs) where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color
import Signal exposing (Address, message)
import Time
import Keyboard
import Debug

type alias Point = (Float, Float)
type alias Vector = (Float, Float)

type alias Model =
 { message : String
 , snake : List Point
 , direction : Vector
 }


type Input = Keyboard {x: Int, y: Int}
           | Tick


init : Model
init = { message = "snake!"
       , snake = [(0, 5), (0, 0)]
       , direction = (0, 1)
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
  -- TODO check valid change, so that it can't go backwards, dot(d1, d2) == 0
  if d.x /= 0 || d.y /= 0 then
    { m | direction <- (toFloat d.x, toFloat d.y) }
  else
    m


moveSnake : Model -> Model
moveSnake m =
  -- for now... need wrap around, wall death, etc.
  { m | snake <- List.map (moveSnakePoint m.direction) m.snake }


moveSnakePoint (dx, dy) (x, y) =
  (x + dx, y + dy)


view : Address Input -> Model -> Element
view a m =
  collage 800 600 (forms m)


forms : Model -> List Form
forms m = [board,
           border,
           snake m.snake]

board : Form
board =
  rect 800 600
    |> filled Color.yellow


border : Form
border =
  let
    initialLineStyle = solid Color.orange
    style = { initialLineStyle | width <- 10 }
  in
    outlined style (rect 800 600)


snake : List Point -> Form
snake s =
  let
    initialListStyle = solid Color.red
    style = { initialListStyle | width <- 10 }
    collagePath = List.map (\(x, y) -> (x * 10, y * 10)) s
  in
    traced style (path collagePath)

