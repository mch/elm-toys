module Aabb exposing (..)

-- External modules

import Dict exposing (..)


-- Internal modules

import EntityId exposing (..)


{-| Performs axis-aligned bounding box collision detection.
-}


{-| An axis-aligned bounding box centred around x,y
-}
type alias Model =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , newOverlaps : List EntityId -- New this update
    , existingOverlaps : List EntityId -- Existed last update, still exist
    }


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


compare2 Model -> Model -> Model
compare2 a b =
    -- 
    a

update : Time -> Time -> Dict.Dict EntityId Model -> Dict.Dict EntityId Model
update time dt aabbs =
    -- compare all items in the dict against each other
    let
        l = Dict.toList aabbs

        compare aabb =
            List.map (compare2 aabb) l
    in
        List.map compare l
            |> Dict.fromList


