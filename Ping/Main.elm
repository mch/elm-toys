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
    }



{- Initialize the model and send initial commands. -}


init : ( Model, Cmd Msg )
init =
    let
        empty =
            Model 0 Components.init Systems.init 0 0 Nothing
    in
        ( { empty | componentData = createTarget ( 100, 100 ) blue empty.componentData }
        , Cmd.none
        )



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

        targets =
            List.map
                (drawTarget model.componentData.fades)
                (Dict.toList model.componentData.targets)

        lasers =
            List.map drawLaser (Dict.toList model.componentData.lasers)

        player =
            Collage.circle 20
                |> filled green

        gameBoard =
            collage collageWidth collageHeight (pings ++ targets ++ lasers ++ [ player ])
                |> toHtml
    in
        div []
            [ gameBoard
            , p [] [ Html.text ("Score: " ++ (toString model.score)) ]
            ]


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
                        |> updateComponentData t
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
