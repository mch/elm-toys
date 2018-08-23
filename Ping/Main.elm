module Main exposing (..)

-- External Modules

import AnimationFrame exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Dict
import Ease
import Element exposing (toHtml)
import Html exposing (..)
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
    }


init : ( Model, Cmd Msg )
init =
    let
        empty =
            Model 0 Components.init Systems.init 0 0 Nothing
    in
        ( { empty | componentData = createTarget ( 100, 100 ) blue empty.componentData }
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

        drawLaser ( id, laser ) =
            segment laser.start laser.end
                |> traced defaultLine

        lasers =
            List.map drawLaser (Dict.toList model.componentData.lasers)

        gameBoard =
            collage collageWidth collageHeight (pings ++ targets ++ lasers)
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
                        |> runSystems t
                        |> updateFromInput t
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
            { componentData
                | targets = missedTargets
            }

        model2 =
            { model | lastClick = Just ( px, py ) }
    in
        if (Dict.size hitTargets > 0) then
            { model2 | componentData = data, score = model.score + points }
        else
            { model2 | componentData = createFadingPing model.componentData model.previousTick ( px, py ) red 30000 }


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
