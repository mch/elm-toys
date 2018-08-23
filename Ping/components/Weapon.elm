module Weapon exposing (..)

import Common exposing (..)
import Tween exposing (..)


type alias Laser =
    { start : Position
    , end : Position
    , damageRate : Float -- damage per second inflicted by this weapon
    , endTime : Float
    }
