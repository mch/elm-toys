module Components exposing (..)

import Ease
import Time exposing (..)


type alias Tween =
    { start : Float
    , end : Float
    , startTime : Time
    , duration : Time
    , f : Ease.Easing
    , value : Float -- Should the component contain this??
    }


createTween : Float -> Float -> Time -> Time -> Ease.Easing -> Tween
createTween start end startTime duration f =
    Tween start end startTime duration f 0.0


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


{-| Applies the easing function for the tween, normalizing inputs and
denormalizing outputs.
-}
updateTween : Tween -> Time -> Tween
updateTween tween t =
    { tween | value = applyEasing tween t }


type Component
    = TweenComponent Tween


updateComponent : Component -> Time -> Component
updateComponent component time =
    case component of
        TweenComponent tween ->
            TweenComponent (updateTween tween time)
