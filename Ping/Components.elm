module Components exposing (..)

-- External modules

import Dict exposing (..)
import Ease
import Time exposing (..)
import Set
import Color exposing (..)


-- Internal modules

import EntityId exposing (..)
import FadeableIntensity exposing (..)
import Health exposing (..)
import Ping exposing (..)
import Target exposing (..)
import Weapon exposing (..)
import Tween exposing (..)
import Common exposing (..)


{-| Possible union type for component data. Could create type alias' for the actual
data in different files.

Does this make it easier or harder to add components? Would make an API that
takes generic components possible, either for adding or removing components from
entities. Or removing all components for a given entity, e.g. if it is dead.

-}
type Component
    = Transformation
        { translate : ( Float, Float )
        , scale : ( Float, Float )
        , rotate : Float
        }
    | AABB2 { width : Float, height : Float }
    | FadeableIntensity2
        { intensity : Float
        , tween : Tween
        }
    | Health2 { health : Int }
    | Ping2
        { color : Color
        , radius : Float
        , speed : Float
        , position : Position
        }
    | Target2
        { color : Color
        , position : Position
        , size : Float
        , value : Int
        }
    | Laser2
        { start : Position
        , end : Position
        , damageRate : Float -- damage per second inflicted by this weapon
        , endTime : Float
        }


{-| Axis aligned bounding box
-}
type alias AABB =
    { width : Float
    , height : Float
    }


{-| A data structure holding all of the component data in the game.

Not sure that the nextEntityId belongs here, but since it needs to be updated if
a new entity is added, it makes sense for it to be here.

It would be nice if adding a component didn't require changing this or init below.

-}
type alias ComponentData =
    { fades : Dict.Dict EntityId FadeableIntensity
    , pings : Dict.Dict EntityId Ping
    , targets : Dict.Dict EntityId Target
    , lasers : Dict.Dict EntityId Laser
    , health : Dict.Dict EntityId Health
    , aabb : Dict.Dict EntityId AABB
    , nextEntityId : EntityId
    }


{-| Set of the component data.
-}
init : ComponentData
init =
    ComponentData
        Dict.empty
        Dict.empty
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


{-| Removes entities that have health <= 0. The implementation is not sustainable:
every new component that is added requires this to be updated.
-}
removeDeadEntities : ComponentData -> ComponentData
removeDeadEntities data =
    let
        deadIds =
            Dict.filter (\id h -> h.health <= 0) data.health
                |> Dict.keys
                |> Set.fromList

        predicate id a =
            Set.member id deadIds
                |> not
    in
        { data
            | fades = Dict.filter predicate data.fades
            , pings = Dict.filter predicate data.pings
            , targets = Dict.filter predicate data.targets
            , lasers = Dict.filter predicate data.lasers
            , health = Dict.filter predicate data.health
        }


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
        |> removeDeadEntities
