module Entities exposing (..)

import Color exposing (..)
import Common exposing (..)
import Components exposing (..)
import Dict exposing (..)
import Ease
import EntityId exposing (..)
import FadeableIntensity exposing (..)
import Ping exposing (..)
import Time exposing (..)
import Tween exposing (..)


createFadingPing : ComponentData -> Time -> Position -> Color -> Time -> ComponentData
createFadingPing data startTime position color duration =
    let
        ping =
            Ping color 10 100 position

        fade =
            FadeableIntensity 1 (Tween 1.0 0.0 startTime duration Ease.linear)
    in
        { data
            | pings = Dict.insert data.nextEntityId ping data.pings
            , fades = Dict.insert data.nextEntityId fade data.fades
            , nextEntityId = data.nextEntityId + 1
        }
