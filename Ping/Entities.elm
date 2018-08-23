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


-- Components

import FadeableIntensity exposing (..)
import Ping exposing (..)
import Target exposing (..)
import Weapon exposing (..)
import Health exposing (..)


{-| Create a ping that fades out over a certain amount of time.
-}
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


{-| Creates a ping reflecting from a target
-}
createSecondaryPing : ComponentData -> Time -> Position -> ComponentData
createSecondaryPing data startTime position =
    let
        pingColour =
            purple

        pingDurationMs =
            1000
    in
        createFadingPing data startTime position pingColour pingDurationMs


{-| Create a target that can take damage at a certain point in space.
-}
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
            , health = Dict.insert id (Health 100) data.health
            , nextEntityId = id + 1
        }


{-| Change the opacity of the target to make it visible, fading out over time
-}
pingTarget : EntityId -> Time -> ComponentData -> ComponentData
pingTarget targetId time data =
    let
        updateTargetFade f =
            { f
                | intensity = 1
                , tween = createTween 1 0 time 1000 Ease.inOutCubic
            }
    in
        { data | fades = Dict.update targetId (Maybe.map updateTargetFade) data.fades }


{-| Create a laser beam that does some damage to a target if one exists at the
end position of the beam.
-}
createLaser : Time -> Position -> Position -> ComponentData -> ComponentData
createLaser time startPosition endPosition data =
    { data
        | lasers = Dict.insert data.nextEntityId (Laser startPosition endPosition 100 (time + 500)) data.lasers
        , nextEntityId = data.nextEntityId + 1
    }
