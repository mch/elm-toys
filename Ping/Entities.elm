module Entities exposing (..)

-- External Modules

import Color exposing (..)
import Dict exposing (..)
import Ease
import Time exposing (..)


-- Internal Modules

import Common exposing (..)
import Components exposing (..)
import EntityId exposing (..)
import Tween exposing (..)


-- Entities

import FadeableIntensity exposing (..)
import Ping exposing (..)
import Target exposing (..)


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


createTarget : Position -> Color -> ComponentData -> ComponentData
createTarget position color data =
    let
        id =
            data.nextEntityId

        target =
            Target color position 20 100

        fade =
            FadeableIntensity 0.0 (createTween 0.0 0.0 0.0 0.0 Ease.linear)
    in
        { data
            | targets = Dict.insert id target data.targets
            , fades = Dict.insert id fade data.fades
            , nextEntityId = id + 1
        }
