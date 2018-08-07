module Main exposing (..)

import AnimationFrame exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Constants exposing (..)
import Dict
import Ease
import Element exposing (toHtml)
import Html exposing (..)
import Mouse
import Task exposing (Task)
import Time exposing (..)
import Window


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
-}


{-| Id that uniquely identifies each entity across all components
-}
type alias EntityId =
    Int


{-| Tween component
-}
type alias Tween =
    { start : Float
    , end : Float
    , startTime : Time
    , duration : Time
    , f : Ease.Easing
    }


{-| Applies the easing function for the tween, normalizing inputs and
denormalizing outputs. This would be the update function for the Tween
component.
-}
applyEasing : Tween -> Time -> Float
applyEasing tween t =
    let
        x =
            (t - tween.startTime) / tween.duration

        b =
            tween.start

        m =
            tween.end - tween.start

        y =
            if t > tween.startTime + tween.duration then
                tween.end
            else
                (tween.f x) * m + b
    in
        y


type alias Position =
    ( Float, Float )



{-
   Components
-}


type alias Ping =
    { color : Color
    , radius : Float
    , speed : Float
    , position : Position
    }


type alias Target =
    { color : Color
    , position : Position
    , size : Float
    , value : Int
    }


type alias FadeableIntensity =
    { intensity : Float
    , tween : Tween
    }


{-| By identifying the overlapping entities first, and modifying them later, we
can perform other actions like creating new entities. This state is related
specifically to the System that detects overlapping entities. It's not really a
component, so I'm not totally sure what to do with it. Systems kind of need a
mechanism to keep track of their own state.
-}
type alias Overlaps =
    List ( EntityId, EntityId )


type alias Model =
    { nextEntityId : EntityId
    , pings : Dict.Dict EntityId Ping
    , targets : Dict.Dict EntityId Target
    , fades : Dict.Dict EntityId FadeableIntensity
    , score : Int
    , previousTick : Time
    , overlaps : Overlaps
    , newOverlaps : Overlaps
    }


createFadingPing : Position -> Color -> Time -> Model -> Model
createFadingPing position color duration model =
    let
        ping =
            Ping color 10 100 position

        fade =
            FadeableIntensity 1 (Tween 1.0 0.0 model.previousTick duration Ease.linear)
    in
        { model
            | pings = Dict.insert model.nextEntityId ping model.pings
            , fades = Dict.insert model.nextEntityId fade model.fades
            , nextEntityId = model.nextEntityId + 1
        }


createTarget : Position -> Color -> Model -> Model
createTarget position color model =
    let
        id =
            model.nextEntityId

        target =
            Target color position 20 100

        fade =
            FadeableIntensity 0.0 (Tween 0.0 0.0 0.0 0.0 Ease.linear)
    in
        { model
            | targets = Dict.insert id target model.targets
            , fades = Dict.insert id fade model.fades
            , nextEntityId = id + 1
        }


init : ( Model, Cmd Msg )
init =
    ( Model 0 Dict.empty Dict.empty Dict.empty 0 0 [] []
        |> createTarget ( 0, 0 ) blue
    , Cmd.none
    )


type Msg
    = Tick Time
    | Click ( Float, Float )


view : Model -> Html Msg
view model =
    let
        drawPing ( id, ping ) =
            Collage.circle ping.radius
                |> outlined { defaultLine | color = (adjustAlpha ping.color (Dict.get id model.fades |> Maybe.map .intensity |> Maybe.withDefault 1)) }
                |> move ping.position

        pings =
            List.map drawPing (Dict.toList model.pings)

        drawTarget ( id, target ) =
            rect target.size target.size
                |> filled (adjustAlpha target.color (Dict.get id model.fades |> Maybe.map .intensity |> Maybe.withDefault 1))
                |> move target.position

        targets =
            List.map drawTarget (Dict.toList model.targets)

        gameBoard =
            collage collageWidth collageHeight (pings ++ targets)
                |> toHtml
    in
        div []
            [ gameBoard
            , p [] [ Html.text ("Score: " ++ (toString model.score)) ]
            ]


adjustAlpha : Color -> Float -> Color
adjustAlpha c i =
    let
        rgb =
            Color.toRgb c
    in
        Color.rgba rgb.red rgb.green rgb.blue i


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    let
        updatePreviousTime t m =
            { m | previousTick = t }

        newModel =
            case action of
                Tick t ->
                    growPings model t
                        |> updateFades t
                        |> detectOverlaps
                        |> handleOverlaps
                        |> updatePreviousTime t

                Click ( px, py ) ->
                    handleClick px py model
    in
        ( newModel, Cmd.none )


handleClick : Float -> Float -> Model -> Model
handleClick px py model =
    let
        detectTarget t =
            let
                ( tx, ty ) =
                    t.position

                halfSize =
                    t.size / 2

                hit =
                    px > tx - halfSize && px < tx + halfSize && py > ty - halfSize && py < ty + halfSize
            in
                ( hit, t )

        identifiedTargets =
            Dict.map (\_ t -> detectTarget t) model.targets

        hitTargets =
            Dict.map (\_ t -> Tuple.second t) (Dict.filter (\_ ( hit, t ) -> hit) identifiedTargets)

        missedTargets =
            Dict.map (\_ t -> Tuple.second t) (Dict.filter (\_ ( hit, t ) -> not hit) identifiedTargets)

        points =
            Dict.foldl (\_ t points -> points + t.value) 0 hitTargets

        -- TODO visual and audio reward for hitting a target
    in
        if (Dict.size hitTargets > 0) then
            { model | targets = missedTargets, score = model.score + points }
        else
            createFadingPing ( px, py ) red 30000 model


updateFades : Time -> Model -> Model
updateFades t model =
    { model | fades = Dict.map (\id f -> { f | intensity = applyEasing f.tween t }) model.fades }


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


detectOverlaps : Model -> Model
detectOverlaps model =
    let
        g pid ping ( tid, target ) =
            if (isTargetDetected target ping) then
                [ ( pid, tid ) ]
            else
                []

        f ( index, ping ) =
            List.concat (List.map (g index ping) (Dict.toList model.targets))

        overlaps =
            List.concat (List.map f (Dict.toList model.pings))

        newOverlaps =
            List.filter (\x -> not (List.member x model.overlaps)) overlaps
    in
        { model | overlaps = overlaps, newOverlaps = newOverlaps }


handleOverlaps : Model -> Model
handleOverlaps model =
    -- Create new ping, mark it as already overlapping with the target that caused it.
    let
        updateTargetFade f =
            { f
                | intensity = 1
                , tween = Tween 1 0 model.previousTick 1000 Ease.inOutCubic
            }

        applyOverlapUpdates ( pid, tid ) model =
            let
                model1 =
                    Dict.get tid model.targets
                        |> Maybe.map (\target -> createFadingPing target.position purple 1000 model)
                        |> Maybe.withDefault model

                updatedOverlaps =
                    ( model.nextEntityId, tid ) :: model1.overlaps

                updatedFades =
                    Dict.update tid (Maybe.map updateTargetFade) model1.fades
            in
                { model1
                    | fades = updatedFades
                    , overlaps = updatedOverlaps
                }
    in
        List.foldl applyOverlapUpdates model model.newOverlaps


{-| This would be the ping component update function.
-}
growPings : Model -> Time -> Model
growPings m t =
    let
        dt =
            (t - m.previousTick) / Time.second

        growPing c =
            { c | radius = c.radius + c.speed * dt }

        newPings =
            Dict.map (\_ p -> growPing p) m.pings

        keptPings =
            Dict.filter (\_ c -> c.radius < maxRadius) newPings
    in
        { m | pings = keptPings }


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
                    ]
        }
