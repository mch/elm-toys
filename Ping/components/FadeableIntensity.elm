module FadeableIntensity exposing (..)

import Tween exposing (..)
import Time exposing (..)


{-| Fadeable intensity component. Add this to a component to change the opacity
of over time.
-}
type alias FadeableIntensity =
    { intensity : Float
    , tween : Tween
    }


updateFade : FadeableIntensity -> Time -> FadeableIntensity
updateFade fade time =
    { fade | intensity = applyEasing fade.tween time }
