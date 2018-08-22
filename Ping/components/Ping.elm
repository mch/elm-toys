module Ping exposing (..)

import Color exposing (..)
import Common exposing (..)
import Time exposing (..)


{- A "ping" is an expanding circle that is used to detect targets in the game world.
   The idea is that when it intersects a target, it causes a secondary ping and causes
   the target to become visible for a certain amount of time.

   The position should maybe be a separate component.
-}


type alias Ping =
    { color : Color
    , radius : Float
    , speed : Float
    , position : Position
    }


maxRadius =
    300


updatePing : Ping -> Time -> Ping
updatePing ping dt =
    { ping
        | radius = ping.radius + ping.speed * dt
    }


pingIsAlive : Ping -> Bool
pingIsAlive p =
    p.radius < maxRadius
