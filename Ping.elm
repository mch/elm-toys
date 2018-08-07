module Main exposing (..)

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


collageWidth =
    800


collageHeight =
    600


maxRadius =
    1000



{- If this was more of a component-entity-system (CES), a Ping or Target would just be
   an integer, the entity id, and there would be components like a drawable component
   which would contain data like shapes, colors, etc, a motion component with speed, a
   system for modifying the drawable based on the speed...
-}


type alias EntityId =
    Int


type alias Tween =
    { start : Float
    , end : Float
    , startTime : Time
    , duration : Time
    , f : Ease.Easing
    }


type alias Ping =
    { color : Color
    , radius : Float
    , speed : Float
    , position : ( Float, Float )
    , fadeSpeed : Float
    , intensity : Float
    }


type alias Target =
    { color : Color
    , position : ( Float, Float )
    , size : Float
    , value : Int

    -- Properties related to drawing the target, not intrinsic to the
    -- target... should be a different record all together?
    , detected : Bool
    , seedNewPing : Bool
    , intensity : Float
    }


type alias FadeableIntensity =
    { intensity : Float
    , tween : Tween
    }



{- By identifying the overlapping entities first, and modifying them later, we
   can perform other actions like creating new entities.
-}


type alias Overlaps =
    List ( EntityId, EntityId )


type OverlapEvent
    = OverlapBegin ( EntityId, EntityId )
    | OverlapEnd ( EntityId, EntityId )


type alias Model =
    { nextEntityId : EntityId
    , pings : Dict.Dict EntityId Ping
    , targets : Dict.Dict EntityId Target
    , score : Int
    , previousTick : Time
    , overlaps : Overlaps
    , newOverlaps : Overlaps
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        1
        Dict.empty
        (Dict.singleton
            0
            (Target blue ( 0, 0 ) 20 100 False False 0)
        )
        0
        0
        []
        []
    , Cmd.none
    )


type Msg
    = Tick Time
    | Click ( Float, Float )


view : Model -> Html Msg
view model =
    let
        drawPing ping =
            Collage.circle ping.radius
                |> outlined { defaultLine | color = (adjustAlpha ping.color ping.intensity) }
                |> move ping.position

        pings =
            List.map drawPing (Dict.values model.pings)

        drawTarget target =
            rect target.size target.size
                |> filled (adjustAlpha target.color target.intensity)
                |> move target.position

        targets =
            List.map drawTarget (Dict.values model.targets)

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
                        |> fadePings t
                        |> fadeTargets t
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
            seedPing px py model


targetDetected : Target -> Ping -> Bool
targetDetected target ping =
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
            if (targetDetected target ping) then
                [ ( pid, tid ) ]
            else
                []

        f ( index, ping ) =
            List.concat (List.map (g index ping) (Dict.toList model.targets))

        overlaps =
            --Debug.log "overlaps"
            (List.concat (List.map f (Dict.toList model.pings)))

        newOverlaps =
            Debug.log "new overlaps" (List.filter (\x -> not (List.member x model.overlaps)) overlaps)
    in
        { model | overlaps = overlaps, newOverlaps = newOverlaps }


handleOverlaps : Model -> Model
handleOverlaps model =
    -- Create new ping, mark it as already overlapping with the target that caused it.
    let
        updateTarget t =
            { t | intensity = 1 }

        updatePing p =
            { p | intensity = p.intensity * 0.9 }

        applyOverlapUpdates ( pid, tid ) ( pings, targets, entityId, overlaps ) =
            let
                updatedPings =
                    Dict.update pid (Maybe.map updatePing) pings
                        |> Dict.insert model.nextEntityId (Ping purple 1 100 ( 0.0, 0.0 ) 1 1)

                updatedOverlaps =
                    ( model.nextEntityId, tid ) :: overlaps

                nextEntityId =
                    entityId + 1

                updatedTargets =
                    Dict.update tid (Maybe.map updateTarget) targets
            in
                ( updatedPings, updatedTargets, nextEntityId, updatedOverlaps )

        ( updatedPings, updatedTargets, nextEntityId, overlaps ) =
            List.foldl applyOverlapUpdates ( model.pings, model.targets, model.nextEntityId, model.overlaps ) model.newOverlaps
    in
        { model | pings = updatedPings, targets = updatedTargets, nextEntityId = nextEntityId, overlaps = overlaps }


fadeTargets : Time -> Model -> Model
fadeTargets t model =
    let
        dt =
            (t - model.previousTick) / Time.second

        fadeSpeed =
            3

        newTargets =
            Dict.map (\_ t -> { t | intensity = t.intensity - fadeSpeed * dt }) model.targets
    in
        { model | targets = newTargets }


fadePings : Time -> Model -> Model
fadePings t model =
    let
        dt =
            (t - model.previousTick) / Time.second

        newPings =
            Dict.map (\_ p -> { p | intensity = p.intensity - p.fadeSpeed * dt }) model.pings

        alivePings =
            Dict.filter (\_ p -> p.intensity > 0) newPings
    in
        { model | pings = alivePings }


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


seedPing : Float -> Float -> Model -> Model
seedPing cx cy m =
    let
        startingRadius =
            10

        startingSpeed =
            100

        defaultFadeSpeed =
            0

        nextEntityId =
            m.nextEntityId + 1

        ping =
            Ping red startingRadius startingSpeed ( cx, cy ) defaultFadeSpeed 1
    in
        { m
            | pings = Dict.insert m.nextEntityId ping m.pings
            , nextEntityId = nextEntityId
        }


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
