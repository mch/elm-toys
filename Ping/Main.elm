module Main exposing (..)

import AnimationFrame exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Common exposing (..)
import Components exposing (..)
import Constants exposing (..)
import Dict
import Ease
import Element exposing (toHtml)
import Entities exposing (..)
import EntityId exposing (..)
import FadeableIntensity exposing (..)
import Html exposing (..)
import Mouse
import Task exposing (Task)
import Time exposing (..)
import Tween exposing (..)
import Window


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
    , componentData : ComponentData
    , score : Int
    , previousTick : Time
    , overlaps : Overlaps
    , newOverlaps : Overlaps
    }


init : ( Model, Cmd Msg )
init =
    let
        empty =
            Model 0 Components.init 0 0 [] []
    in
        ( { empty | componentData = createTarget ( 0, 0 ) blue empty.componentData }
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
                |> outlined { defaultLine | color = (adjustAlpha ping.color (Dict.get id model.componentData.fades |> Maybe.map .intensity |> Maybe.withDefault 1)) }
                |> move ping.position

        pings =
            List.map drawPing (Dict.toList model.componentData.pings)

        drawTarget ( id, target ) =
            rect target.size target.size
                |> filled (adjustAlpha target.color (Dict.get id model.componentData.fades |> Maybe.map .intensity |> Maybe.withDefault 1))
                |> move target.position

        targets =
            List.map drawTarget (Dict.toList model.componentData.targets)

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


updateComponentData : Time -> Model -> Model
updateComponentData t model =
    let
        dt =
            (t - model.previousTick) / Time.second
    in
        { model | componentData = updateComponents model.componentData t dt }


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    let
        updatePreviousTime t m =
            { m | previousTick = t }

        newModel =
            case action of
                Tick t ->
                    model
                        |> updateComponentData t
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
            Dict.map (\_ t -> detectTarget t) model.componentData.targets

        hitTargets =
            Dict.map (\_ t -> Tuple.second t) (Dict.filter (\_ ( hit, t ) -> hit) identifiedTargets)

        missedTargets =
            Dict.map (\_ t -> Tuple.second t) (Dict.filter (\_ ( hit, t ) -> not hit) identifiedTargets)

        points =
            Dict.foldl (\_ t points -> points + t.value) 0 hitTargets

        componentData =
            model.componentData

        -- TODO visual and audio reward for hitting a target
        data =
            { componentData | targets = missedTargets }
    in
        if (Dict.size hitTargets > 0) then
            { model | componentData = data, score = model.score + points }
        else
            { model | componentData = createFadingPing model.componentData model.previousTick ( px, py ) red 30000 }


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


{-| This System detects overlapping pings and other objects and updates all
interacting entities as needed.
-}
detectOverlaps : Model -> Model
detectOverlaps model =
    let
        g pid ping ( tid, target ) =
            if (isTargetDetected target ping) then
                [ ( pid, tid ) ]
            else
                []

        f ( index, ping ) =
            List.concat (List.map (g index ping) (Dict.toList model.componentData.targets))

        overlaps =
            List.concat (List.map f (Dict.toList model.componentData.pings))

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
                , tween = createTween 1 0 model.previousTick 1000 Ease.inOutCubic
            }

        applyOverlapUpdates ( pid, tid ) model =
            let
                model1 =
                    Dict.get tid model.componentData.targets
                        |> Maybe.map (\target -> { model | componentData = createFadingPing model.componentData model.previousTick target.position purple 1000 })
                        |> Maybe.withDefault model

                updatedOverlaps =
                    ( model.componentData.nextEntityId, tid ) :: model1.overlaps

                updatedFades =
                    Dict.update tid (Maybe.map updateTargetFade) model1.componentData.fades

                componentData =
                    model1.componentData

                componentData1 =
                    { componentData | fades = updatedFades }
            in
                { model1
                    | componentData = componentData1
                    , overlaps = updatedOverlaps
                }
    in
        List.foldl applyOverlapUpdates model model.newOverlaps


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
