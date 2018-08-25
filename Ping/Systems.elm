module Systems exposing (..)

-- External Modules

import Time exposing (..)


-- Internal Modules

import Components exposing (..)
import EntityId exposing (..)
import PingTargets
import DamageTargets


{-| System data: I wonder if this should just be a "special" component & entity.

Threading this in is awkward.

-}
type alias SystemData =
    { pingedTargets : PingTargets.Overlaps
    }


init =
    { pingedTargets = [] }


runSystems : Time -> Time -> ( SystemData, ComponentData ) -> ( SystemData, ComponentData )
runSystems time dt ( systemData, componentData ) =
    let
        ( updatedPingedTargets, updatedComponentData ) =
            PingTargets.run time dt ( systemData.pingedTargets, componentData )
    in
        ( { systemData | pingedTargets = updatedPingedTargets }, DamageTargets.run time dt updatedComponentData )
