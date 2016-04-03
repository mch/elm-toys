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


type alias Circle =
  { color : Color
  , radius : Float
  , speed : Float
  , position : ( Float, Float )
  }


type alias Target =
  { color: Color
  , position : ( Float, Float )
  , size : Float
  , detected : Bool
  }
      

type alias Model =
  { circles : List Circle
  , targets : List Target
  , previousTick : Time
  }


type Action
  = Tick Time
  | Frame Time
  | SeedCircle ( Float, Float )


view : Signal.Address Action -> Model -> Html
view address model =
  let
    drawCircle circle =
      Graphics.Collage.circle circle.radius
        |> outlined { defaultLine | color = circle.color }
        |> move circle.position

    circles =
      List.map drawCircle model.circles

    drawTarget t =
      rect t.size t.size
        |> filled blue
        |> move t.position

    targets =
      List.map drawTarget (List.filter (\t -> t.detected) model.targets)
  in
    collage collageWidth collageHeight (circles ++ targets)
      |> fromElement


update : Action -> Model -> ( Model, Effects.Effects Action )
update a m =
  let
    updatePreviousTime t m =
      { m | previousTick = t }

    newModel =
      case a of
        Frame dt ->
          growCircles m dt
            |> detectTargets

        Tick t ->
          growCircles m t
            |> detectTargets
            |> updatePreviousTime t

        SeedCircle ( cx, cy ) ->
          seedCircle cx cy m

  in
    ( newModel, Effects.tick Tick )


detectTargets : Model -> Model
detectTargets m =
  let
    targetDetected t c =
      let
        (cx, cy) = c.position
        (tx, ty) = t.position
        (dx, dy) = (abs (cx - tx), abs (cy - ty))
        d = sqrt (dx^2 + dy^2)
        min = c.radius - t.size
        max = c.radius + t.size
      in
        d > min && d < max

    detectTarget circles t =
      let
        detected = List.foldl (||) False (List.map (targetDetected t) circles)
      in
          { t | detected = detected }


    updatedTargets = Debug.log "targets" (List.map (detectTarget m.circles) m.targets)
  in
    { m | targets = updatedTargets }

      
growCircles : Model -> Time -> Model
growCircles m t =
  let
    dt =
      (t - m.previousTick) / Time.second

    growCircle c =
      { c | radius = c.radius + c.speed * dt }

    newCircles =
      List.map growCircle m.circles

    keptCircles =
      List.filter (\c -> c.radius < maxRadius) newCircles
  in
    { m | circles = keptCircles }


seedCircle : Float -> Float -> Model -> Model
seedCircle cx cy m =
  let
    startingRadius =
      10

    startingSpeed =
      100
  in
    { m | circles = (Circle red startingRadius startingSpeed ( cx, cy )) :: m.circles }


init : ( Model, Effects.Effects Action )
init =
  ( Model [] [Target blue (0, 0) 10 False] 0, Effects.tick Tick )


mouseToCollage : ( Int, Int ) -> ( Int, Int ) -> ( Float, Float )
mouseToCollage ( mx, my ) ( wx, wy ) =
  -- Forgot about getting window dimensions initially being 'hard'
  ( toFloat (mx - (collageWidth // 2)), toFloat ((collageHeight // 2) - my) )


inputs =
  Signal.map2 mouseToCollage Mouse.position Window.dimensions
    |> Signal.map SeedCircle


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
