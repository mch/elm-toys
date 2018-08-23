module PingTargets exposing (..)

-- External Modules

import Color exposing (..)
import Dict exposing (..)
import Time exposing (..)


-- Internal Modules

import Components exposing (..)
import Entities exposing (..)
import EntityId exposing (..)
import Ping exposing (..)
import Target exposing (..)


{-| By identifying the overlapping entities first, and modifying them later, we
can perform other actions like creating new entities. This state is related
specifically to the System that detects overlapping entities. It's not really a
component, so I'm not totally sure what to do with it. Systems kind of need a
mechanism to keep track of their own state.
-}
type alias Overlaps =
    List ( EntityId, EntityId )


{-| This System detects overlapping pings and other objects and updates all
interacting entities as needed.
-}
run : Time -> Time -> ( Overlaps, ComponentData ) -> ( Overlaps, ComponentData )
run time dt ( overlaps, componentData ) =
    let
        newOverlaps =
            detectOverlaps overlaps componentData

        ( updatedOverlaps, updatedComponents ) =
            handleOverlaps time newOverlaps overlaps componentData
    in
        ( List.append newOverlaps updatedOverlaps, updatedComponents )


detectOverlaps : Overlaps -> ComponentData -> Overlaps
detectOverlaps existingOverlaps componentData =
    let
        g pid ping ( tid, target ) =
            if (isTargetDetected target ping) then
                [ ( pid, tid ) ]
            else
                []

        f ( index, ping ) =
            List.concat (List.map (g index ping) (Dict.toList componentData.targets))

        overlaps =
            List.concat (List.map f (Dict.toList componentData.pings))

        newOverlaps =
            List.filter (\x -> not (List.member x existingOverlaps)) overlaps
    in
        newOverlaps


handleOverlaps : Time -> Overlaps -> Overlaps -> ComponentData -> ( Overlaps, ComponentData )
handleOverlaps time newOverlaps existingOverlaps data =
    -- Create new ping, mark it as already overlapping with the target that caused it.
    let
        applyOverlapUpdates ( pid, tid ) ( overlaps, componentData ) =
            let
                updatedComponents =
                    Dict.get tid componentData.targets
                        |> Maybe.map (\target -> createSecondaryPing componentData time target.position)
                        |> Maybe.withDefault componentData

                updatedOverlaps =
                    ( componentData.nextEntityId, tid ) :: overlaps

                updatedComponents2 =
                    pingTarget tid time updatedComponents
            in
                ( updatedOverlaps, updatedComponents2 )
    in
        List.foldl applyOverlapUpdates ( existingOverlaps, data ) newOverlaps


isTargetDetected : Target -> Ping -> Bool
isTargetDetected target ping =
    let
        ( px, py ) =
            ping.position

        ( tx, ty ) =
            target.position

        ( dx, dy ) =
            ( abs (px - tx), abs (py - ty) )

        d =
            sqrt (dx ^ 2 + dy ^ 2)

        min =
            ping.radius - target.size

        max =
            ping.radius + target.size
    in
        d > min && d < max
