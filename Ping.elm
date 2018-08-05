module Main exposing (..)

import AnimationFrame exposing (..)
import Collage exposing (..)
import Element exposing (toHtml)
import Task exposing (Task)
import Color exposing (..)
import Time exposing (..)
import Html exposing (..)
import Mouse
import Window


collageWidth =
    800


collageHeight =
    600


maxRadius =
    1000



-- If this was more of a component-entity-system (CES), a Ping or Target would just be
-- an integer, the entity id, and there would be components like a drawable component
-- which would contain data like shapes, colors, etc, a motion component with speed, a
-- system for modifying the drawable based on the speed.


type alias EntityId =
    Int


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



-- When a ping first overlaps a target, it results in a OverlapBegin event.
-- When it stops overlapping, a OverlapEnd event. This record keeps track of
-- the current overlaps as part of detecting overlaps and emitting events.


type alias Overlaps =
    List ( EntityId, EntityId )



-- I wonder if this should be more generic, where the events contain Ints
-- that are used to look up the actual data in a Dict or List.


type OverlapEvent
    = OverlapBegin Ping Target
    | OverlapEnd Ping Target


type alias Model =
    { pings : List Ping
    , targets : List Target
    , overlaps : Overlaps
    , score : Int
    , previousTick : Time
    }


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
            List.map drawPing model.pings

        drawTarget target =
            rect target.size target.size
                |> filled (adjustAlpha target.color target.intensity)
                |> move target.position

        targets =
            List.map drawTarget model.targets

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
                        |> detectTargets
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
            List.map detectTarget model.targets

        hitTargets =
            List.map Tuple.second (List.filter (\( hit, t ) -> hit) identifiedTargets)

        missedTargets =
            List.map Tuple.second (List.filter (\( hit, t ) -> not hit) identifiedTargets)

        points =
            List.foldl (\t points -> points + t.value) 0 hitTargets

        -- TODO visual and audio reward for hitting a target
    in
        if (List.length hitTargets > 0) then
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


detectTargets : Model -> Model
detectTargets model =
    let
        detectTarget pings target =
            let
                detected =
                    List.foldl (||) False (List.map (targetDetected target) pings)

                firstDetection =
                    detected && (not target.detected)

                intensity =
                    if detected then
                        1
                    else
                        target.intensity
            in
                { target
                    | detected = detected
                    , seedNewPing = firstDetection
                    , intensity = intensity
                }

        updatedTargets =
            List.map (detectTarget model.pings) model.targets

        generatePing position =
            let
                startingRadius =
                    1

                speed =
                    100

                fadeSpeed =
                    1

                color =
                    purple
            in
                Ping color startingRadius speed position fadeSpeed 1

        newPings =
            List.map (\t -> generatePing t.position) (List.filter (\t -> t.seedNewPing) updatedTargets)
    in
        { model | targets = updatedTargets, pings = model.pings ++ newPings }


fadeTargets : Time -> Model -> Model
fadeTargets t model =
    let
        dt =
            (t - model.previousTick) / Time.second

        fadeSpeed =
            3

        newTargets =
            List.map (\t -> { t | intensity = t.intensity - fadeSpeed * dt }) model.targets
    in
        { model | targets = newTargets }


fadePings : Time -> Model -> Model
fadePings t model =
    let
        dt =
            (t - model.previousTick) / Time.second

        newPings =
            List.map (\p -> { p | intensity = p.intensity - p.fadeSpeed * dt }) model.pings

        alivePings =
            List.filter (\p -> p.intensity > 0) newPings
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
            List.map growPing m.pings

        keptPings =
            List.filter (\c -> c.radius < maxRadius) newPings
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
    in
        { m | pings = (Ping red startingRadius startingSpeed ( cx, cy ) defaultFadeSpeed 1) :: m.pings }


init : ( Model, Cmd Msg )
init =
    ( Model [] [ Target blue ( 0, 0 ) 20 100 False False 0 ] [] 0 0, Cmd.none )


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
