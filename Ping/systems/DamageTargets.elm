module DamageTargets exposing (..)

-- External Modules

import Dict exposing (..)
import Time exposing (..)


-- Internal Modules

import Components exposing (..)
import Target exposing (..)
import Weapon exposing (..)


{-| Check if two aabb's at given positions intersect each other.

        xA1       xA2
    yA2 +---------+
        |         |
        |         |
        |         |
        |  xA,yA  |
        |    *----|----* yB2
        |    |    |    |
        |    |    |    |
        |    |    |    |
    yA1 +----+----+    |
             |  xB,yB  |
             |         |
             |         |
             *---------* yB1
             xB1       xB2

-}
aabbIntersection ( xA, yA ) bbA ( xB, yB ) bbB =
    let
        xA1 =
            xA - bbA.width / 2.0

        xA2 =
            xA + bbA.width / 2.0

        yA1 =
            yA - bbA.height / 2.0

        yA2 =
            yA + bbA.height / 2.0

        xB1 =
            xB - bbB.width / 2.0

        xB2 =
            xB + bbB.width / 2.0

        yB1 =
            yB - bbB.height / 2.0

        yB2 =
            yB + bbB.height / 2.0
    in
        -- TODO unit tests
        xB2 >= xA1 && xB1 <= xA2 && yB2 >= yA1 && yB1 <= yA1


run : Time -> Time -> ComponentData -> ComponentData
run time dt componentData =
    let
        -- Doing something to compare two sets of components is going to be a
        -- common structure I think, should try to extract it. It's used in
        -- PingTargets too.
        doesLaserHitTarget ( laserId, laser ) ( targetId, target ) =
            Dict.get targetId componentData.aabb
                |> Maybe.map (\targetAabb -> aabbIntersection laser.end { width = 2.0, height = 2.0 } target.position targetAabb)
                |> Maybe.withDefault False
                |> (\hit ->
                        if hit then
                            [ ( laserId, laser, targetId, target ) ]
                        else
                            []
                   )

        testLaserHit laser =
            List.concat (List.map (doesLaserHitTarget laser) (Dict.toList componentData.targets))

        hits =
            List.concat (List.map testLaserHit (Dict.toList componentData.lasers))

        reducer ( laserId, laser, targetId, target ) targets =
            let
                updatedTarget =
                    { target | health = target.health - laser.damageRate }
            in
                Dict.insert targetId updatedTarget targets

        updatedTargets =
            List.foldl reducer componentData.targets hits
                |> Dict.filter (\_ target -> target.health > 0)

        -- Find targets intersecting laser
        -- Add damage to the target
        -- Increment the score and blow up the target if it's damage reaches it threshold.
    in
        { componentData | targets = updatedTargets }
