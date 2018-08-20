module Components exposing (..)

import Dict exposing (..)
import Ease
import EntityId exposing (..)
import FadeableIntensity exposing (..)
import Ping exposing (..)
import Target exposing (..)
import Time exposing (..)


{-| A data structure holding all of the component data in the game.

Not sure that the nextEntityId belongs here...

-}
type alias ComponentData =
    { fades : Dict.Dict EntityId FadeableIntensity
    , pings : Dict.Dict EntityId Ping
    , targets : Dict.Dict EntityId Target
    , nextEntityId : EntityId
    }


init : ComponentData
init =
    ComponentData
        Dict.empty
        Dict.empty
        Dict.empty
        0


{-| updateComponents needs both an absolute time and a time delta, since
different components need different timing for their updates. Updating components
in general might be better as individual systems.
-}
updateComponents : ComponentData -> Time -> ComponentData
updateComponents components time =
    { components
        | fades = Dict.map (\_ f -> updateFade f time) components.fades
        , pings = Dict.map (\_ p -> updatePing p time) components.pings

        --, targets = Dict.map (\_ t -> updateTargets t time) components.targets
    }
