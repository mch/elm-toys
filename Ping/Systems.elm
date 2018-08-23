module Systems exposing (..)

-- External Modules

import Time exposing (..)


-- Internal Modules

import Components exposing (..)
import EntityId exposing (..)
import PingTargets


{-| System data
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
        ( { systemData | pingedTargets = updatedPingedTargets }, updatedComponentData )
