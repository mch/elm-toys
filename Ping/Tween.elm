module Tween exposing (..)

import Ease
import Time exposing (..)


{-| Move a value between one value and another over time. There is a library
called animation which is pretty much the same, but with an incremental construction
API which I might steal.
-}
type alias Tween =
    { start : Float
    , end : Float
    , startTime : Time
    , duration : Time
    , f : Ease.Easing
    }


{-| Create a new tween.
-}
createTween : Float -> Float -> Time -> Time -> Ease.Easing -> Tween
createTween start end startTime duration f =
    Tween start end startTime duration f


{-| Applies the easing function for the tween, normalizing inputs and
denormalizing outputs.
-}
applyEasing : Tween -> Time -> Float
applyEasing tween t =
    let
        x =
            (t - tween.startTime) / tween.duration

        b =
            tween.start

        m =
            tween.end - tween.start

        y =
            if t > tween.startTime + tween.duration then
                tween.end
            else
                (tween.f x) * m + b
    in
        y
