import Signal exposing (..)
import Mouse
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Window
import Time exposing (Time, fps, inMilliseconds)
import Easing exposing (..) 

-- Inspired by http://conal.net/fran/tutorial.htm
-- Trying to reproduce the composition there. Didn't realize the nature
-- of wiggle and waggle on the first pass. 

type Input = Tick Time

type AnimationState = Stopped | Once | Loop

type alias AnimationModel = { elapsed: Time, duration: Time, state: AnimationState }

looping : Time -> AnimationModel
looping d = { elapsed = 0, duration = d, state = Loop }

type alias Model = { a: List AnimationModel }

init : Model
init = { a = [looping (Time.second * 5), looping (Time.second * 2), looping (Time.second * 2) ] }

update : Input -> Model -> Model
update i m = 
  case i of
    Tick dt -> { m | a <- List.map (updateAnimation dt) m.a }

updateAnimation : Time -> AnimationModel -> AnimationModel
updateAnimation dt m = 
  if m.elapsed >= m.duration then
    { m | elapsed <- 0 }
  else
    { m | elapsed <- m.elapsed + dt }

main = Signal.map view (Signal.foldp update init (map Tick delay))

view : Model -> Element
view m = 
  let 
    r a f = rotate (a.elapsed / a.duration * 2 * 3.14159) f
    wx a f = moveX (a.elapsed / a.duration * 100) f
    wy a f = moveY ((a.elapsed / a.duration - 0.5) * 100) f
    jiggleX a f = moveX (-200 + (ease (retour easeInOutQuad) float 0 400 a.duration a.elapsed)) f
    jiggleY a f = moveY (ease (retour easeInOutQuad) float -200 200 a.duration a.elapsed) f
    pt = List.map2 (\f x -> f x) [r, jiggleX, jiggleY] m.a
    t = List.foldl (>>) identity pt
  in
    collage 500 500 [t img]


delay = Time.fps 30

img = toForm (image 100 100 "http://i.imgur.com/L7wtdwT.gif")

