module Main exposing (..)

-- External Modules

import AnimationFrame exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Debug
import Dict
import Ease
import Element exposing (toHtml)
import Html exposing (..)
import Keyboard
import Mouse
import Set
import Task exposing (Task)
import Time exposing (..)
import Window


collageWidth =
    800


collageHeight =
    600

type alias EntityId =
    Int


type alias Model =
    { score : Int
    , previousTick : Time
    , lastClick : Maybe Vec2
    , newComponentData : ComponentData
    , keysDown : Set.Set Keyboard.KeyCode
    , level : Int
    }


{-| A collection which relates an EntityId to the data for a particular type of component.

This and the functions below should perhaps be extracted to a module.

The internal representation is currently a Dict, but an Array might be a better
choice, along with some tools to help keep track of the next available EntityId,
give that some entities won't last long and their spot in the arrays should be
reused.

-}
type alias EntityComponents component = Dict.Dict EntityId component

{-| Apply a function to one component of an entity, for each of a given list of entity ids.

Takes care of extracting the component data and calling the provided function on
it. If the component is missing, the function is not called.

Example use, drawing a list of entities on a canvas.
    drawEntity : Transformation -> Form
    drawEntity t =
        Collage.circle 20
            |> filled green
            |> move (t.translation.x, t.translation.y)

    applyToEntity drawEntity [0, 1] data.transformation
-}
applyToEntity : (a -> result)
              -> List EntityId
              -> EntityComponents a
              -> List result
applyToEntity f ids a = []

applyToEntity2 : (a -> b -> result)
               -> List EntityId
               -> EntityComponents a
               -> EntityComponents b
               -> List result
applyToEntity2 f ids a b = []

applyToEntity3 : (a -> b -> c -> result)
               -> List EntityId
               -> EntityComponents a
               -> EntityComponents b
               -> EntityComponents c
               -> List result
applyToEntity3 f ids a b c = []

applyToEntity4 : (a -> b -> c -> d -> result)
               -> List EntityId
               -> EntityComponents a
               -> EntityComponents b
               -> EntityComponents c
               -> EntityComponents d
               -> List result
applyToEntity4 f ids a b c d = []

{-| Update the state for an entity.

Extracts and unwraps the required component data, calling the given function on
it.
 -}
updateEntity : EntityId -> (a -> data) -> EntityComponents a -> data -> data
updateEntity id f a data = data

updateEntity2 : EntityId
              -> (a -> b -> data)
              -> EntityComponents a
              -> EntityComponents b
              -> data
              -> data
updateEntity2 id f a b data = data

updateEntity3 : EntityId
              -> (a -> b -> c -> data)
              -> EntityComponents a
              -> EntityComponents b
              -> EntityComponents c
              -> data
              -> data
updateEntity3 id f a b c data = data

updateEntity4 : EntityId
              -> (a -> b -> c -> d -> data)
              -> EntityComponents a
              -> EntityComponents b
              -> EntityComponents c
              -> EntityComponents d
              -> data
              -> data
updateEntity4 id f a b c d data = data

{-| Keeps track of all data related to entities.

Includes data for specific components and data for systems that relate multiple
entities or components.

-}
type alias ComponentData =
    { nextEntityId : EntityId
    , entityType : EntityComponents EntityType
    , transformation : EntityComponents Transformation
    , lifeCycle : EntityComponents LifeCycle
    , pingable : EntityComponents Pingable
    , boundingCircle : EntityComponents Float
    , path : EntityComponents Path
    , ping : EntityComponents Ping
    , boundary : EntityComponents Boundary

    -- The following fields are for keeping track of how entities interact with each other.
    , newBoundingCircleOverlaps : List (EntityId, EntityId, EntityType, EntityType)
    , existingBoundingCircleOverlaps : List (EntityId, EntityId, EntityType, EntityType)

    -- A list of events that might be of interest to the higher level game, used
    -- to update the score, change levels, and so on.
    , events : List GameEvent
    }

initComponentData =
    { nextEntityId = 0
    , entityType = Dict.empty
    , transformation = Dict.empty
    , lifeCycle = Dict.empty
    , pingable = Dict.empty
    , boundingCircle = Dict.empty
    , path = Dict.empty
    , ping = Dict.empty
    , boundary = Dict.empty

    -- System data
    , newBoundingCircleOverlaps = []
    , existingBoundingCircleOverlaps = []

    -- Game events
    , events = []
    }

type NewComponent
    = TransformationComponent Transformation
    | LifeCycleComponent LifeCycle
    | PingableComponent Pingable
    | BoundingCircleComponent Float
    | PathComponent Path
    | PingComponent Ping
    | BoundaryComponent Boundary

type alias Transformation =
    { translation : Vec2
    , scale : Vec2
    , rotate : Vec2
    }

type alias LifeCycle =
    { birthTime : Time
    , ttl : Maybe Time -- Number of ms to live, or Nothing to live forever.
    }

type alias Pingable =
    { pingTime : Time
    }

type Path = None
          | Circle Vec2 Float
          | Ellipse Vec2 Float Float Float
          | Lissajous Vec2 Float Float Float Float Float
          | Hypotrochoid
          | Velocity Vec2

type alias Ping =
    { radius : Float
    , color : Color
    }

type Boundary = WrapAroundBoundary
              | DestructionBoundary

type GameEvent = TargetDestroyed

type alias Vec2 = {x: Float, y: Float}

type EntityType = PlayerEntity | TargetEntity | PingEntity | ProjectileEntity | ExplosionEntity

setEntityType : EntityId -> EntityType -> ComponentData -> ComponentData
setEntityType id entityType data =
    { data | entityType = Dict.insert id entityType data.entityType }

getEntitiesOfType : EntityType -> ComponentData -> List EntityId
getEntitiesOfType entityType data =
    Dict.filter (\k v -> v == entityType) data.entityType
        |> Dict.keys

incrementNextEntityId : ComponentData -> ComponentData
incrementNextEntityId data =
    { data | nextEntityId = data.nextEntityId + 1 }

addComponent : EntityId -> NewComponent -> ComponentData -> ComponentData
addComponent id comp data =
    case comp of
        TransformationComponent componentData ->
            { data | transformation = Dict.insert id componentData data.transformation }

        LifeCycleComponent componentData ->
            { data | lifeCycle = Dict.insert id componentData data.lifeCycle }

        PingableComponent componentData ->
            { data | pingable = Dict.insert id componentData data.pingable }

        BoundingCircleComponent radius ->
            { data | boundingCircle = Dict.insert id radius data.boundingCircle }

        PathComponent path ->
            { data | path = Dict.insert id path data.path }

        PingComponent ping ->
            { data | ping = Dict.insert id ping data.ping }

        BoundaryComponent boundary ->
            { data | boundary = Dict.insert id boundary data.boundary }

createPlayerEntity : Time -> EntityId -> ComponentData -> ComponentData
createPlayerEntity t id data =
    let
        transformation = TransformationComponent (Transformation (Vec2 0 0) (Vec2 1 1) (Vec2 0 0))
    in
        data
        |> addComponent id transformation
        |> addComponent id (BoundaryComponent WrapAroundBoundary)

createTargetEntity : Vec2 -> Path -> Time -> EntityId -> ComponentData -> ComponentData
createTargetEntity position path t id data =
    let
        transformation = TransformationComponent (Transformation position (Vec2 1 1) (Vec2 0 0))

        pingable = PingableComponent (Pingable 0)
    in
        data
        |> addComponent id transformation
        |> addComponent id (PathComponent path)
        |> addComponent id pingable
        |> addComponent id (BoundingCircleComponent 20)

createPingEntity : Vec2 -> Time -> Time -> EntityId -> ComponentData -> ComponentData
createPingEntity position ttl t id data =
    data
        |> addComponent id (BoundingCircleComponent 10)
        |> addComponent id (PingComponent (Ping 10 red))
        |> addComponent id (TransformationComponent (Transformation position (Vec2 1 1) (Vec2 0 0)))
        |> addComponent id (LifeCycleComponent (LifeCycle t (Just (ttl * second))))


createProjectileEntity : Vec2 -> Vec2 -> Time -> EntityId -> ComponentData -> ComponentData
createProjectileEntity position velocity time id data =
    data
        |> addComponent id (BoundingCircleComponent 5)
        |> addComponent id (TransformationComponent (Transformation position (Vec2 1 1) (Vec2 0 0)))
        |> addComponent id (PathComponent (Velocity velocity))
        |> addComponent id (BoundaryComponent DestructionBoundary)


createExplosionEntity : Vec2 -> Time -> EntityId -> ComponentData -> ComponentData
createExplosionEntity position time id data =
    data
        |> addComponent id (BoundingCircleComponent 10)
        |> addComponent id (TransformationComponent (Transformation position (Vec2 0 0) (Vec2 0 0)))
        |> addComponent id (LifeCycleComponent (LifeCycle time (Just 1000)))


updatePingEntity : Time -> EntityId -> ComponentData -> ComponentData
updatePingEntity dt id data =
    let
        update radius =
            Maybe.map (\r -> r + dt / 10) radius

        updatePing ping =
            Maybe.map (\p -> { p | radius = p.radius + dt / 10 }) ping
    in
        { data | boundingCircle = Dict.update id update data.boundingCircle
        , ping = Dict.update id updatePing data.ping }

type alias EntityCreator = (Time -> EntityId -> ComponentData -> ComponentData)

createEntity : Time -> EntityType -> EntityCreator -> ComponentData -> ComponentData
createEntity t et f data =
    let
        id = data.nextEntityId
    in
        -- Set up the defaults before calling the entity creator, then it can
        -- modify them if it needs to.
        data
            |> addComponent id (LifeCycleComponent (LifeCycle t Nothing))
            |> setEntityType id et
            |> f t id
            |> incrementNextEntityId


updateComponentData : Time -> Model -> Model
updateComponentData t model =
    let
        originalData =
            model.newComponentData

        data =
            { originalData | events = [] }

        pingEntities = getEntitiesOfType PingEntity data

        filterOldPings t data =
            let
                playAreaRadius = 1200
                oldPings = Dict.filter (\id radius -> radius > playAreaRadius) data.boundingCircle
                         |> Dict.keys
            in
                deleteEntities oldPings data

        filterDeadEntities t data =
            let
                entities = Dict.filter (\id lifeCycle ->
                                       case lifeCycle.ttl of
                                           Nothing -> False
                                           Just ttl ->
                                               if t > lifeCycle.birthTime + ttl then
                                                   True
                                               else
                                                   False
                                       ) data.lifeCycle
                           |> Dict.keys
            in
                deleteEntities entities data

        dt = t - model.previousTick

        updatedData =
            List.foldl (updatePingEntity dt) data pingEntities
                |> filterOldPings t
                |> filterDeadEntities t
                |> detectOverlaps
                |> processTargets t
                |> followPaths t dt
                |> handleBoundaries
    in
        { model | newComponentData = updatedData }
            |> processEvents t


processEvents : Time -> Model -> Model
processEvents time model =
    let
        nextLevel model =
            { model | level = model.level + 1 }
                |> createLevelEntities time

        processEvent event model =
            case event of
                TargetDestroyed ->
                    let
                        remainingTargetCount =
                            getEntitiesOfType TargetEntity model.newComponentData
                                |> List.length

                        updatedModel =
                            { model | score = model.score + 1 }
                    in
                        if remainingTargetCount == 0 then
                            nextLevel updatedModel
                        else
                            updatedModel
    in
        List.foldl processEvent model model.newComponentData.events


handleBoundaries data =
    let
        applyWrapAround transform =
            let
                maxX =
                    collageWidth / 2

                minX =
                    -1 * collageWidth / 2

                maxY =
                    collageHeight / 2

                minY =
                    -1 * collageHeight / 2

                adjustment value min max foo =
                    if value > max then
                        value - foo
                    else if value < min then
                        value + foo
                    else
                        value
            in
                { transform | translation =
                      Vec2
                      (adjustment transform.translation.x minX maxX collageWidth)
                      (adjustment transform.translation.y minY maxY collageHeight)
                }

        applyDestruction id data =
            let
                checkBounds t =
                    if t.translation.x < -1 * collageWidth / 2
                        || t.translation.x > collageWidth / 2
                        || t.translation.y < -1 * collageHeight / 2
                        || t.translation.y > collageHeight / 2 then
                        deleteEntities [id] data
                    else
                        data
            in
                Dict.get id data.transformation
                    |> Maybe.map checkBounds
                    |> Maybe.withDefault data

        applyBoundary id boundary data =
            case boundary of
                WrapAroundBoundary ->
                    { data | transformation = updateComponent id applyWrapAround data.transformation }
                DestructionBoundary ->
                    applyDestruction id data
    in
        Dict.foldl applyBoundary data data.boundary


-- A helper function for updating component data that unwraps the Maybe from the Dict.
updateComponent : EntityId -> (a -> a) -> (EntityComponents a) -> (EntityComponents a)
updateComponent id f d =
    Dict.update id (\item -> Maybe.map f item) d


-- Finds overlapping entities by checking their bounding circles, and stores the results.
detectOverlaps : ComponentData -> ComponentData
detectOverlaps data =
    let
        entities =
            Dict.keys data.boundingCircle

        lookupComponents id =
            Maybe.map3 (\et t radius -> (id, et, t.translation, radius))
                (Dict.get id data.entityType)
                (Dict.get id data.transformation)
                (Dict.get id data.boundingCircle)

        -- Sort the list of bounding circles by id so that we can skip comparing
        -- the same items twice
        circleComparitor (id1, _, _, _) (id2, _, _, _) =
            if id1 == id2 then
                EQ
            else
                if id1 > id2 then
                     GT
                 else
                     LT

        circles =
            List.filterMap lookupComponents entities
                |> List.sortWith circleComparitor

        -- This solution is a bit sub-optimal because it traverses the list
        -- twice, but the number of items is small for now, so let's roll with
        -- it.
        overlaps =
            List.concatMap (\c1 -> List.map (\c2 -> compareCircles c1 c2) circles) circles
                |> List.filterMap identity

        newOverlaps =
            List.filter (\x -> not (List.member x data.existingBoundingCircleOverlaps)) overlaps

    in
        { data | newBoundingCircleOverlaps = newOverlaps
        , existingBoundingCircleOverlaps = overlaps
        }

compareCircles (id1, et1, t1, r1) (id2, et2, t2, r2) =
    if id1 == id2 || id1 > id2 then
        Nothing
    else
        let
            (dx, dy) =
                (abs (t2.x - t1.x), abs (t2.y - t1.y))

            d =
                sqrt (dx ^ 2 + dy ^ 2)
        in
            -- Currently this is only when the outer edges intersect. If one
            -- circle is fully within another, that is not considered an
            -- intersection, because that is not what we want for pings.
            if r1 + r2 > d && r2 < d + r1 && r1 < d + r2 then
                Just (id1, id2, et1, et2)
            else
                Nothing


-- Processes targets, creating new reflected pings if they were just pinged,
-- moving them, handling damage from weapons, etc.
processTargets : Time -> ComponentData -> ComponentData
processTargets t data =
    let
        entities = getEntitiesOfType TargetEntity data
    in
        List.foldl (processTarget t) data entities

processTarget time id data =
    let
        transform = Dict.get id data.transformation
        path = Dict.get id data.path
        lifeCycle = Dict.get id data.lifeCycle
    in
        Maybe.withDefault data (Maybe.map3 (processTarget2 id time data) transform path lifeCycle)

processTarget2 id time data transform path lifeCycle =
    let
        handleOverlaps translation (id1, id2, et1, et2) data =
            if (id1 == id || id2 == id) && (et1 == PingEntity || et2 == PingEntity) then
                -- It would be cool if the initial opacity of the ping related
                -- to the current opacity of the ping that caused this
                -- reflection... But the original pings don't fade very fast
                -- right now.
                let
                    newPingId = data.nextEntityId
                in
                    createEntity time PingEntity (createPingEntity translation 1) data
                        |> (\data -> { data | boundingCircle = Dict.remove newPingId data.boundingCircle })
                        |> updatePingTime id time
            else if (id1 == id || id2 == id) && (et1 == ProjectileEntity || et2 == ProjectileEntity) then
                let
                    newId = data.nextEntityId
                in
                    createEntity time ExplosionEntity (createExplosionEntity translation) data
                        |> deleteEntities [id1, id2] -- One hit kill for now
                        |> (\data -> { data | events = TargetDestroyed :: data.events } )
            else
                data
    in
        List.foldl (handleOverlaps transform.translation) data data.newBoundingCircleOverlaps


followPaths : Time -> Time -> ComponentData -> ComponentData
followPaths t dt data =
    let
        paths = Dict.keys data.path

        update id data =
            let
                path = Dict.get id data.path
                transform = Dict.get id data.transformation
                lifeCycle = Dict.get id data.lifeCycle
            in
                Maybe.withDefault data (Maybe.map3 (updateTransformFollowingPath id t dt data) path transform lifeCycle)
    in
        List.foldl update data paths


updateTransformFollowingPath id time dt data path transform lifeCycle =
    case path of
        None ->
            data
        Circle centre radius ->
            let
                angle =
                    (time - lifeCycle.birthTime) / 1000

                translation =
                    Vec2 (centre.x + radius * (cos angle))
                        (centre.y + radius * (sin angle))
            in
                { data | transformation = updateComponent id (\t -> { t | translation = translation }) data.transformation }

        Ellipse centre major minor phi ->
            let
                angle =
                    (time - lifeCycle.birthTime) / 1000

                translation =
                    Vec2 (centre.x + major * (cos angle) * (cos phi) - minor * (sin angle) * (sin phi))
                        (centre.y + major * (cos angle) * (sin phi) - minor * (sin angle) * (cos phi))
            in
                { data | transformation = updateComponent id (\t -> { t | translation = translation }) data.transformation }

        Lissajous centre kx ky major minor phi->
            let
                angle =
                    (time - lifeCycle.birthTime) / 1000

                majorAngle =
                    kx * angle

                minorAngle =
                    ky * angle

                translation =
                    Vec2 (centre.x + major * (cos majorAngle) * (cos phi) - minor * (sin minorAngle) * (sin phi))
                        (centre.y + major * (cos majorAngle) * (sin phi) - minor * (sin minorAngle) * (cos phi))
            in
                { data | transformation = updateComponent id (\t -> { t | translation = translation }) data.transformation }

        Hypotrochoid ->
            data
        Velocity velocity ->
            let
                updateTransform t =
                    { t | translation = Vec2 (t.translation.x + velocity.x * dt) (t.translation.y + velocity.y * dt) }
            in
                { data | transformation = updateComponent id (\t -> updateTransform t) data.transformation}


updatePingTime id t data =
    { data | pingable = updateComponent id (\p -> { p | pingTime = t }) data.pingable }


-- Removes all components for each entity in the list.
deleteEntities : List EntityId -> ComponentData -> ComponentData
deleteEntities entities data =
    let
        -- How do we ensure this gets updated if a new component is added?
        deleteEntity id data =
            { data | entityType = Dict.remove id data.entityType
            , transformation = Dict.remove id data.transformation
            , lifeCycle = Dict.remove id data.lifeCycle
            , pingable = Dict.remove id data.pingable
            , boundingCircle = Dict.remove id data.boundingCircle
            }
    in
        List.foldl deleteEntity data entities


{- Initialize the model and send initial commands. -}


init : ( Model, Cmd Msg )
init =
    let
        empty =
            Model 0 0 Nothing initComponentData Set.empty 0
    in
        ( empty, Cmd.none )


{- Creates the entities for a level. -}
createLevelEntities : Time -> Model -> Model
createLevelEntities t model =
    if model.level == 0 then
        {model | newComponentData = model.newComponentData
             |> createEntity t PlayerEntity createPlayerEntity
             |> createEntity t TargetEntity (createTargetEntity (Vec2 0 -150) None)
             |> createEntity t PingEntity (createPingEntity (Vec2 0 0) 10)
        }
    else if model.level == 1 then
        {model | newComponentData = model.newComponentData
             |> createEntity t PlayerEntity createPlayerEntity
             |> createEntity t TargetEntity (createTargetEntity (Vec2 0 -150) None)
             |> createEntity t TargetEntity (createTargetEntity (Vec2 0 150) None)
        }
    else if model.level == 2 then
        {model | newComponentData = model.newComponentData
             |> createEntity t PlayerEntity createPlayerEntity
             |> createEntity t TargetEntity (createTargetEntity (Vec2 0 -150) None)
             |> createEntity t TargetEntity (createTargetEntity (Vec2 0 150) None)
             |> createEntity t TargetEntity (createTargetEntity (Vec2 150 0) None)
             |> createEntity t TargetEntity (createTargetEntity (Vec2 -150 0) None)
        }
    else if model.level == 3 then
        {model | newComponentData = model.newComponentData
             |> createEntity t PlayerEntity createPlayerEntity
             |> createEntity t TargetEntity (createTargetEntity (Vec2 -50 -50) (Circle (Vec2 0 0) 250))
             |> createEntity t TargetEntity (createTargetEntity (Vec2 -50 -50) (Circle (Vec2 0 0) 200))
             |> createEntity t TargetEntity (createTargetEntity (Vec2 -50 -50) (Circle (Vec2 0 0) 150))
        }
    else
        {model | newComponentData = model.newComponentData
             |> createEntity t PlayerEntity createPlayerEntity
             |> createEntity t TargetEntity (createTargetEntity (Vec2 -50 -50) (Circle (Vec2 0 0) 250))
             |> createEntity t TargetEntity (createTargetEntity (Vec2 -50 -50) (Ellipse (Vec2 -50 0) 200 150 0))
             |> createEntity t TargetEntity (createTargetEntity (Vec2 -50 -50) (Ellipse (Vec2 50 0) 200 150 0))
             |> createEntity t TargetEntity (createTargetEntity (Vec2 -50 -50) (Lissajous (Vec2 0 0) 2 3 300 250 0))
        }


{- Creates initial level entities after the first time tick so that they have sensible lifecycle info. -}
createInitialEntities : Time -> Model -> Model
createInitialEntities t model =
    if model.previousTick == 0 then
        createLevelEntities t model
    else
        model

{- Messages that can come into the update function. -}


type Msg
    = Tick Time
    | Click ( Float, Float )
    | KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode



{- View the model as an HTML page and helper functions -}


view : Model -> Html Msg
view model =
    let
        pings =
            getEntitiesOfType PingEntity model.newComponentData
                |> List.map (drawPing2 model.previousTick model.newComponentData)
                |> List.filterMap identity

        targets =
            getEntitiesOfType TargetEntity model.newComponentData
                |> List.map (drawTarget2 model.previousTick model.newComponentData)
                |> List.filterMap identity

        player =
            getEntitiesOfType PlayerEntity model.newComponentData
                |> List.map (drawPlayer model.newComponentData)
                |> List.filterMap identity

        projectiles =
            getEntitiesOfType ProjectileEntity model.newComponentData
                |> List.map (drawProjectile model.newComponentData)
                |> List.filterMap identity

        explosions =
            getEntitiesOfType ExplosionEntity model.newComponentData
                |> List.map (drawExplosion model.previousTick model.newComponentData)
                |> List.filterMap identity

        gameBoard =
            collage collageWidth collageHeight (targets ++ player ++ pings ++ projectiles ++ explosions)
                |> toHtml
    in
        div []
            [ gameBoard
            , p [] [ Html.text ("Score: " ++ (toString model.score)) ]
            ]


drawPlayer data id =
    let
        t = Dict.get id data.transformation
        l = Dict.get id data.lifeCycle

        draw t l =
            Collage.circle 20
                |> filled green
                |> move (t.translation.x, t.translation.y)
    in
        Maybe.map2 draw t l

drawTarget2 time data id =
    let
        t = Dict.get id data.transformation
        l = Dict.get id data.lifeCycle
        p = Dict.get id data.pingable

        draw t l p =
            rect 20 20
                |> filled (adjustAlpha blue (Ease.linear (1 - (time - p.pingTime) / 1000)))
                |> move (t.translation.x, t.translation.y)
    in
        Maybe.map3 draw t l p

-- This one just extracts component data... can this be generalized?
drawPing2 time data id =
    let
        transform = Dict.get id data.transformation
        ping = Dict.get id data.ping
        lifeCycle = Dict.get id data.lifeCycle
    in
        Maybe.map3 (drawPing3 time) transform ping lifeCycle

-- This one does the actual drawing...
drawPing3 time transform ping lifeCycle =
    let
        ttl = Maybe.withDefault (100000 * millisecond) lifeCycle.ttl
        easing = 1 - (time - lifeCycle.birthTime) / ttl
        fade = Ease.linear easing
        translation = transform.translation
    in
            Collage.circle ping.radius
                |> outlined { defaultLine | color = (adjustAlpha ping.color fade) }
                |> move (translation.x, translation.y)


drawProjectile data id =
    let
        t = Dict.get id data.transformation

        draw t =
            Collage.circle 5
                |> filled red
                |> move (t.translation.x, t.translation.y)
    in
        Maybe.map draw t


drawExplosion time data id =
    let
        t = Dict.get id data.transformation
        lifeCycle = Dict.get id data.lifeCycle

        draw t lifeCycle =
            let
                ttl = Maybe.withDefault (1 * second) lifeCycle.ttl
                dt = (time - lifeCycle.birthTime) / ttl
                radius = 20 * (Ease.inOutBounce dt)
            in
                Collage.circle radius
                    |> filled yellow
                    |> move (t.translation.x, t.translation.y)
    in
        Maybe.map2 draw t lifeCycle

adjustAlpha : Color -> Float -> Color
adjustAlpha c i =
    let
        rgb =
            Color.toRgb c
    in
        Color.rgba rgb.red rgb.green rgb.blue i



{- Update the model by responding to incoming messages. -}


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    let
        updatePreviousTime t m =
            { m | previousTick = t }

        newModel =
            case action of
                Tick t ->
                    model
                        |> createInitialEntities t
                        |> updateComponentData t
                        |> updateFromInput t
                        |> updatePreviousTime t

                Click ( px, py ) ->
                    handleClick px py model

                KeyDown code ->
                    handleKeyDown code model

                KeyUp code ->
                    handleKeyUp code model
    in
        ( newModel, Cmd.none )


updateFromInput : Time -> Model -> Model
updateFromInput time model =
    case model.lastClick of
        Just position ->
            { model
                | lastClick = Nothing
                , newComponentData = shootProjectile model.previousTick position model.newComponentData
            }

        Nothing ->
            model

getPlayerPosition data =
    List.head (getEntitiesOfType PlayerEntity data)
        |> Maybe.andThen (\id -> Dict.get id data.transformation)
        |> Maybe.map (\t -> t.translation)
        |> Maybe.withDefault (Vec2 0 0)

shootProjectile time position data =
    let
        playerPosition =
            getPlayerPosition data

        v =
            Vec2 (position.x - playerPosition.x) (position.y - playerPosition.y)

        magnitude =
            sqrt ( v.x ^ 2 + v.y ^ 2)

        normalized =
            Vec2 (v.x / magnitude) (v.y / magnitude)

        projectileSpeed =
            1.0

        velocity =
            Vec2 (normalized.x * projectileSpeed) (normalized.y * projectileSpeed)
    in
        createEntity time ProjectileEntity (createProjectileEntity playerPosition velocity) data

handleClick : Float -> Float -> Model -> Model
handleClick px py model =
    { model | lastClick = Just ( Vec2 px py ) }


handleKeyDown code model =
    -- When the key goes down, set the player's Path to a Velocity, and when it
    -- goes up, remove that component.
    --
    -- Add something for boundary conditions, e.g. the player's boundary
    -- condition might be to slow down as it approaches the boundary, while a
    -- projectiles might be to continue out and then have it's entity deleted.
    -- The player might also wrap around to the other side.

    if not (Set.member code model.keysDown) then
    -- e: 69
    -- w: 87
    -- a: 65
    -- s: 83
    -- d: 68
        if (Debug.log "keycode:" code) == 69 then
            { model | newComponentData = pingPlayer model.previousTick model.newComponentData
            , keysDown = Set.insert code model.keysDown
            }
        else if code == 87 || code == 65 || code == 83 || code == 68 then
            { model | newComponentData = setPlayerVelocity code model.newComponentData
            , keysDown = Set.insert code model.keysDown
            }
        else
            model
    else
        model

handleKeyUp code model =
    -- TODO handle the case where multiple keys are down and only one is lifted,
    -- so that the player still keeps moving.
    if code == 87 || code == 65 || code == 83 || code == 68 then
        { model | newComponentData = removePlayerVelocity model.newComponentData
        , keysDown = Set.remove code model.keysDown
        }
    else
        { model | keysDown = Set.remove code model.keysDown }

pingPlayer time data =
    let
        playerPosition =
            getPlayerPosition data
    in
        createEntity time PingEntity (createPingEntity playerPosition 10) data

setPlayerVelocity code data =
    let
        speed = 1

        xVelocity =
            if code == 65 then
                -speed
            else if code == 68 then
                speed
            else
                0

        yVelocity =
            if code == 83 then
                -speed
            else if code == 87 then
                speed
            else
                0

        playerId =
            List.head (getEntitiesOfType PlayerEntity data)

        existingPath =
            Maybe.andThen (\id -> Dict.get id data.path) playerId
                |> Maybe.withDefault (Velocity (Vec2 0 0))

        newPath =
            case existingPath of
                Velocity v ->
                    Velocity (Vec2 (v.x + xVelocity) (v.y + yVelocity))
                _ ->
                    Velocity (Vec2 xVelocity yVelocity)

        updatePlayer id =
            addComponent id (PathComponent newPath) data
    in
        Maybe.withDefault data (Maybe.map updatePlayer playerId)


removePlayerVelocity data =
    let
        playerId = List.head (getEntitiesOfType PlayerEntity data)

        removePath id =
            { data | path = Dict.remove id data.path }
    in
        Maybe.withDefault data (Maybe.map removePath playerId)

mouseToCollage : ( Int, Int ) -> ( Int, Int ) -> ( Float, Float )
mouseToCollage ( mx, my ) ( wx, wy ) =
    -- Forgot about getting window dimensions initially being 'hard'
    ( toFloat (mx - (collageWidth // 2)), toFloat ((collageHeight // 2) - my) )


clickSub =
    Sub.map Click (Mouse.clicks (\p -> mouseToCollage ( p.x, p.y ) ( 640, 480 )))


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions =
            \_ ->
                Sub.batch
                    [ AnimationFrame.times Tick
                    , clickSub
                    , Keyboard.downs KeyDown
                    , Keyboard.ups KeyUp
                    ]
        }
