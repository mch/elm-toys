module Components exposing (..)

import Dict exposing (..)
import Ease
import EntityId exposing (..)
import FadeableIntensity exposing (..)
import Weapon exposing (..)
import Ping exposing (..)
import Target exposing (..)
import Time exposing (..)


{-| A data structure holding all of the component data in the game.

Not sure that the nextEntityId belongs here, but since it needs to be updated if
a new entity is added, it makes sense for it to be here.

-}
type alias ComponentData =
    { fades : Dict.Dict EntityId FadeableIntensity
    , pings : Dict.Dict EntityId Ping
    , targets : Dict.Dict EntityId Target
    , lasers : Dict.Dict EntityId Laser
    , nextEntityId : EntityId
    }


init : ComponentData
init =
    ComponentData
        Dict.empty
        Dict.empty
        Dict.empty
        Dict.empty
        100000


updatePings : Time -> Dict.Dict EntityId Ping -> Dict.Dict EntityId Ping
updatePings dt pings =
    Dict.map (\_ p -> updatePing p dt) pings
        |> Dict.filter (\_ p -> p.radius < maxRadius)


updateLasers : Time -> Dict.Dict EntityId Laser -> Dict.Dict EntityId Laser
updateLasers time lasers =
    Dict.filter (\_ l -> l.endTime > time) lasers


{-| updateComponents needs both an absolute time and a time delta, since
different components need different timing for their updates. Updating components
in general might be better as individual systems.
-}
updateComponents : Time -> Time -> ComponentData -> ComponentData
updateComponents time dt components =
    { components
        | fades = Dict.map (\_ f -> updateFade f time) components.fades
        , pings = updatePings dt components.pings
        , lasers = updateLasers time components.lasers
    }
