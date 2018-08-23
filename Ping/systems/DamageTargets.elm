module DamageTargets exposing (..)

import Target exposing (..)
import Weapon exposing (..)

run : Time -> Time -> ( SystemData, ComponentData ) -> ( SystemData, ComponentData )
run time dt (systemData, componentData) =
    let
        -- Find targets intersecting laser
        -- Add damage to the target
        -- Increment the score and blow up the target if it's damage reaches it threshold.
    in
        (systemData, componentData)
