module Snake (Model, Input, view, update, init) where

import Graphics.Element exposing (..)
import Signal exposing (Address, message)

type alias Model = { message : String }

type Input = Left | Right | Up | Down

init : Model
init = { message = "snake!" }

update : Input -> Model -> Model
update i m = 
  case i of 
    Left -> { m | message <- "Snake left" }
    Right -> { m | message <- "Snake right" }
    Up -> { m | message <- "Snake up" }
    Down -> { m | message <- "Snake down" }

view : Address Input -> Model -> Element
view a m = show m.message
