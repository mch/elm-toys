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
import Task exposing (Task)
import Time exposing (..)
import Window


-- Internal Modules

import Common exposing (..)
import Components exposing (..)
import Constants exposing (..)
import Entities exposing (..)
import EntityId exposing (..)
import FadeableIntensity exposing (..)
import Tween exposing (..)
import Systems exposing (..)


{- Components -}

import Ping exposing (..)
import Target exposing (..)


{- If this was more of a component-entity-system (CES), a Ping or Target would
      just be an integer, the entity id, and there would be components like a
      drawable component which would contain data like shapes, colors, etc, a
      motion component with speed, a system for modifying the drawable based on
      the speed...

   TODO
   - Add functions to clear an entity from all component collections
   - Make it easier to add an entity that involves multiple components
   - Make it easier to add an entity that only involves one component, because of
     the need to update nextEntityId
   - Entity life cycle: how do we know when to delete a component that is finished?
-}


type alias Model =
    { nextEntityId : EntityId
    , componentData : ComponentData
    , systemData : SystemData
    , score : Int
    , previousTick : Time
    , lastClick : Maybe Position
    -- Another shot at more generic components
    , newComponentData : NewComponentData
    }

{- A different way of representing component data.

Rather than having a lot of specialized components, create more generic
components that can be more broadly used, and add a dictionary to keep track of
the type of entity so that rendering functions and systems know what to do with
the component data that forms a particular type of entity.

-}

type alias NewComponentData =
    { nextEntityId : EntityId
    , entityType : Dict.Dict EntityId EntityType
    , transformation : Dict.Dict EntityId Transformation -- Translation, scale, rotation
    , lifeCycle : Dict.Dict EntityId LifeCycle -- When an entity was first created, how long it should live, etc.
    , pingable : Dict.Dict EntityId Pingable
    , boundingCircle : Dict.Dict EntityId Float
    }

initNewComponentData =
    { nextEntityId = 0
    , entityType = Dict.empty
    , transformation = Dict.empty
    , lifeCycle = Dict.empty
    , pingable = Dict.empty
    , boundingCircle = Dict.empty
    }

type NewComponent
    = TransformationComponent Transformation
    | LifeCycleComponent LifeCycle
    | PingableComponent Pingable
    | BoundingCircleComponent Float

type alias Transformation =
    { translation : Vec2
    , scale : Vec2
    , rotate : Vec2
    }

type alias LifeCycle =
    { birthTime : Time
    , ttl : Maybe Time -- Number of ms to live, or Nothing to live forever.
    , state : LifeCycleState
    }

type alias Pingable =
    { pingTime : Maybe Time
    }

type LifeCycleState =
    Alive | Dead

type alias Vec2 = {x: Float, y: Float}

type EntityType = PlayerEntity | TargetEntity | PingEntity

setEntityType : EntityId -> EntityType -> NewComponentData -> NewComponentData
setEntityType id entityType data =
    { data | entityType = Dict.insert id entityType data.entityType }

getEntitiesOfType : EntityType -> NewComponentData -> List EntityId
getEntitiesOfType entityType data =
    Dict.filter (\k v -> v == entityType) data.entityType
        |> Dict.keys

incrementNextEntityId : NewComponentData -> NewComponentData
incrementNextEntityId data =
    { data | nextEntityId = data.nextEntityId + 1 }

addComponent : EntityId -> NewComponent -> NewComponentData -> NewComponentData
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

createPlayerEntity : Time -> EntityId -> NewComponentData -> NewComponentData
createPlayerEntity t id data =
    let
        transformation = TransformationComponent (Transformation (Vec2 0 0) (Vec2 1 1) (Vec2 0 0))
    in
        data
        |> addComponent id transformation

createTargetEntity : Time -> EntityId -> NewComponentData -> NewComponentData
createTargetEntity t id data =
    let
        transformation = TransformationComponent (Transformation (Vec2 -50 -50) (Vec2 1 1) (Vec2 0 0))

        pingable = PingableComponent (Pingable Nothing)
    in
        data
        |> addComponent id transformation
        |> addComponent id pingable
        |> addComponent id (BoundingCircleComponent 20)

createPingEntity : Vec2 -> Time -> EntityId -> NewComponentData -> NewComponentData
createPingEntity position t id data =
    data
        |> addComponent id (BoundingCircleComponent 20)
        |> addComponent id (TransformationComponent (Transformation position (Vec2 1 1) (Vec2 0 0)))
        |> addComponent id (LifeCycleComponent (LifeCycle t (Just (10 * second)) Alive))


updatePingEntity : Time -> EntityId -> NewComponentData -> NewComponentData
updatePingEntity dt id data =
    let
        update radius =
            Maybe.map (\r -> r + 100 * dt) radius
    in
        { data | boundingCircle = Dict.update id update data.boundingCircle }

type alias EntityCreator = (Time -> EntityId -> NewComponentData -> NewComponentData)

createEntity : Time -> EntityType -> EntityCreator -> NewComponentData -> NewComponentData
createEntity t et f data =
    let
        id = data.nextEntityId
    in
        -- Set up the defaults before calling the entity creator, then it can
        -- modify them if it needs to.
        data
            |> addComponent id (LifeCycleComponent (LifeCycle t Nothing Alive))
            |> setEntityType id et
            |> f t id
            |> incrementNextEntityId


updateNewComponentData : Time -> Model -> Model
updateNewComponentData t model =
    let
        data = model.newComponentData

        pingEntities = getEntitiesOfType PingEntity data

        filterOldPings t data =
            let
                oldPings = Dict.filter (\id radius -> radius > 150) data.boundingCircle
                         |> Dict.keys
            in
                deleteEntities oldPings data

        dt = t - model.previousTick

        updatedData = List.foldl (updatePingEntity dt) data pingEntities
                     |> filterOldPings t
    in
        { model | newComponentData = updatedData }

deleteEntities : List EntityId -> NewComponentData -> NewComponentData
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
            Model 0 Components.init Systems.init 0 0 Nothing initNewComponentData
    in
        ( { empty | componentData = createTarget ( 100, 100 ) blue empty.componentData }
        , Cmd.none
        )


{- Creates entities the first time the time is updated so that they have proper birth times. -}
createInitialEntities : Time -> Model -> Model
createInitialEntities t model =
    if model.previousTick == 0 then
        {model | newComponentData = model.newComponentData
             |> createEntity t PlayerEntity createPlayerEntity
             |> createEntity t TargetEntity createTargetEntity
             |> createEntity t PingEntity (createPingEntity (Vec2 0 0))
        }
    else
        model

{- Messages that can come into the update function. -}


type Msg
    = Tick Time
    | Click ( Float, Float )
    | KeyDown Keyboard.KeyCode



{- View the model as an HTML page and helper functions -}


view : Model -> Html Msg
view model =
    let
        pings =
            List.map
                (drawPing model.componentData.fades)
                (Dict.toList model.componentData.pings)

        pings2 =
            getEntitiesOfType PingEntity model.newComponentData
                |> List.map (drawPing2 model.previousTick model.newComponentData)
                |> List.filterMap identity

        targets =
            List.map
                (drawTarget model.componentData.fades)
                (Dict.toList model.componentData.targets)

        targets2 =
            getEntitiesOfType TargetEntity model.newComponentData
                |> List.map (drawTarget2 model.newComponentData)
                |> List.filterMap identity

        lasers =
            List.map drawLaser (Dict.toList model.componentData.lasers)

        player =
            getEntitiesOfType PlayerEntity model.newComponentData
                |> List.map (drawPlayer model.newComponentData)
                |> List.filterMap identity

        gameBoard =
            collage collageWidth collageHeight (pings ++ targets ++ targets2 ++ lasers ++ player ++ pings2)
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

drawTarget2 data id =
    let
        t = Dict.get id data.transformation
        l = Dict.get id data.lifeCycle

        draw t l =
            rect 50 50
                |> filled blue
                |> move (t.translation.x, t.translation.y)
    in
        Maybe.map2 draw t l

drawPing fades ( id, ping ) =
    let
        fade =
            Dict.get id fades
                |> Maybe.map .intensity
                |> Maybe.withDefault 1
    in
        Collage.circle ping.radius
            |> outlined { defaultLine | color = (adjustAlpha ping.color fade) }
            |> move ping.position


-- This one just extracts component data... can this be generalized?
drawPing2 time data id =
    let
        transform = Dict.get id data.transformation
        boundingCircle = Dict.get id data.boundingCircle
        lifeCycle = Dict.get id data.lifeCycle
    in
        Maybe.map3 (drawPing3 time) transform boundingCircle lifeCycle

-- This one does the actual drawing...
drawPing3 time transform boundingCircle lifeCycle =
    let
        ttl = Maybe.withDefault (100000 * millisecond) lifeCycle.ttl
        easing = 1 - (time - lifeCycle.birthTime) / ttl
        fade = Ease.linear easing
        translation = transform.translation
    in
            Collage.circle boundingCircle
                |> outlined { defaultLine | color = (adjustAlpha red fade) }
                |> move (translation.x, translation.y)

drawTarget fades ( id, target ) =
    let
        fade =
            Dict.get id fades
                |> Maybe.map .intensity
                |> Maybe.withDefault 1
    in
        rect target.size target.size
            |> filled (adjustAlpha target.color fade)
            |> move target.position


drawLaser ( id, laser ) =
    segment laser.start laser.end
        |> traced defaultLine


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
                        |> updateNewComponentData t
                        |> runSystems t
                        |> updateFromInput t
                        |> updatePreviousTime t

                Click ( px, py ) ->
                    handleClick px py model

                KeyDown code ->
                    handleKey code model
    in
        ( newModel, Cmd.none )


updateFromInput : Time -> Model -> Model
updateFromInput time model =
    case model.lastClick of
        Just position ->
            { model
                | componentData = createLaser time ( 0.0, 0.0 ) position model.componentData
                , lastClick = Nothing
            }

        Nothing ->
            model


updateComponentData : Time -> Model -> Model
updateComponentData t model =
    let
        dt =
            (t - model.previousTick) / Time.second
    in
        { model | componentData = updateComponents t dt model.componentData }


runSystems : Time -> Model -> Model
runSystems t model =
    let
        dt =
            (t - model.previousTick) / Time.second

        ( updatedSystems, updatedComponents ) =
            Systems.runSystems t dt ( model.systemData, model.componentData )
    in
        { model
            | componentData = updatedComponents
            , systemData = updatedSystems
        }


handleClick : Float -> Float -> Model -> Model
handleClick px py model =
    { model | lastClick = Just ( px, py ) }


handleKey code model =
    -- e: 69
    -- w: 87
    -- a: 65
    -- s: 83
    -- d: 68
    if (Debug.log "keycode:" code) == 69 then
        { model
            | componentData = createFadingPing model.componentData model.previousTick ( 0, 0 ) red 30000
            , newComponentData = createEntity model.previousTick PingEntity (createPingEntity (Vec2 0 0)) model.newComponentData
        }
    else
        model


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
                    ]
        }
