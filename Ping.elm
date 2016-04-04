module Main (..) where

import Graphics.Collage exposing (..)
import StartApp exposing (start)
import Effects exposing (tick, none)
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


type alias Ping =
  { color : Color
  , radius : Float
  , speed : Float
  , position : ( Float, Float )
  , fadeSpeed : Float
  }


type alias Target =
  { color : Color
  , position : ( Float, Float )
  , size : Float
  , detected : Bool
  , seedNewPing : Bool
  }


type alias Model =
  { pings : List Ping
  , targets : List Target
  , previousTick : Time
  }


type Action
  = Tick Time
  | Frame Time
  | SeedPing ( Float, Float )


view : Signal.Address Action -> Model -> Html
view address model =
  let
    drawPing ping =
      Graphics.Collage.circle ping.radius
        |> outlined { defaultLine | color = ping.color }
        |> move ping.position

    pings =
      List.map drawPing model.pings

    drawTarget target =
      rect target.size target.size
        |> filled blue
        |> move target.position

    targets =
      List.map drawTarget (List.filter (\t -> t.detected) model.targets)
  in
    collage collageWidth collageHeight (pings ++ targets)
      |> fromElement


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  let
    updatePreviousTime t m =
      { m | previousTick = t }

    newModel =
      case action of
        Frame dt ->
          growPings model dt
            |> fadePings dt
            |> detectTargets

        Tick t ->
          growPings model t
            |> fadePings t
            |> detectTargets
            |> updatePreviousTime t

        SeedPing ( px, py ) ->
          seedPing px py model
  in
    ( newModel, Effects.tick Tick )


detectTargets : Model -> Model
detectTargets model =
  let
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

    detectTarget pings target =
      let
        detected =
          List.foldl (||) False (List.map (targetDetected target) pings)

        firstDetection = detected && (not target.detected)
      in
        { target
          | detected = detected
          , seedNewPing = firstDetection
        }

    updatedTargets =
      List.map (detectTarget model.pings) model.targets


    generatePing position  =
      let
        startingRadius =
          1

        speed =
          100

        fadeSpeed =
          2

        color =
          purple
      in
        Ping color startingRadius speed position fadeSpeed

          
    newPings = List.map (\t -> generatePing t.position) (List.filter (\t -> t.seedNewPing) updatedTargets)
  in
    { model | targets = updatedTargets, pings = model.pings ++ newPings }


fadePings : Time -> Model -> Model
fadePings t model =
  let
    dt = (t - model.previousTick) / Time.second

    fadeColor color amount =
      let 
        hslValues = Color.toHsl color
      in
        Color.hsla hslValues.hue hslValues.saturation hslValues.lightness (hslValues.alpha - amount)

    newPings = List.map (\p -> { p | color = fadeColor p.color (p.fadeSpeed * dt) }) model.pings
    alivePings = List.filter (\p -> (toHsl p.color).alpha > 0) newPings
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
    { m | pings = (Ping red startingRadius startingSpeed ( cx, cy ) defaultFadeSpeed) :: m.pings }


init : ( Model, Effects.Effects Action )
init =
  ( Model [] [ Target blue ( 0, 0 ) 20 False False ] 0, Effects.tick Tick )


mouseToCollage : ( Int, Int ) -> ( Int, Int ) -> ( Float, Float )
mouseToCollage ( mx, my ) ( wx, wy ) =
  -- Forgot about getting window dimensions initially being 'hard'
  ( toFloat (mx - (collageWidth // 2)), toFloat ((collageHeight // 2) - my) )


inputs =
  Signal.map2 mouseToCollage Mouse.position Window.dimensions
    |> Signal.map SeedPing


sampledInputs =
  Signal.sampleOn Mouse.clicks inputs



-- Boilerplate


app =
  start { init = init, view = view, update = update, inputs = [ sampledInputs {- , Signal.map Frame (Time.fps 30) -} ] }


main =
  app.html



-- Debugger can't be used with ports, and thus Effects.tick can't be
-- used for animation. Thus the "Frame" action.


port runner : Signal (Task Effects.Never ())
port runner =
  app.tasks
