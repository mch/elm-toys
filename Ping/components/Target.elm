module Target exposing (..)

import Common exposing (..)
import Color exposing (..)


{-| This is mostly for drawing something at a particular position, so "Target"
is probably the wrong name for it.
-}
type alias Target =
    { color : Color
    , position : Position
    , size : Float
    , value : Int
    , health : Float
    }
