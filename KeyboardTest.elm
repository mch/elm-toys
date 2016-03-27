import Keyboard
import Html exposing (..)
import Signal exposing (..)
import Time exposing (fps)
import Dict


type Action = Tick Float
            | Wasd { x : Int, y : Int }
            | Input Inputs


type alias Model =
  { numW : Int
  , numA : Int
  , numS : Int
  , numD : Int
  , numDelta : Int
  , lastDelta : Float
  }


init =
  Model 0 0 0 0 0 0


update action model =
  case action of
    Tick dt -> updateTicks { dt = dt } model
    Wasd wasd -> updateKeys wasd model
    Input inputs -> updateTicks inputs (updateKeys inputs model)


updateTicks inputs model =
  { model
    | numDelta = model.numDelta + 1
    , lastDelta = inputs.dt
  }

updateKeys wasd model =
  let
    w = if wasd.y == 1 then 1 else 0
    s = if wasd.y == -1 then 1 else 0
    a = if wasd.x == -1 then 1 else 0
    d = if wasd.x == 1 then 1 else 0
  in
    { model
      | numW = model.numW + w
      , numA = model.numA + a
      , numS = model.numS + s
      , numD = model.numD + d
    }


view model =
  --let
  --m = Dict.map (\k v -> Dict.singleton k (toString v)) model
  --in
  div [] [ h1 [] [ text "Inputs" ]
         , ul [] [ li [] [ text ("Number of times W has been pressed: " ++ (toString model.numW)) ]
                 , li [] [ text ("Number of times A has been pressed: " ++ (toString model.numA)) ]
                 , li [] [ text ("Number of times S has been pressed: " ++ (toString model.numS)) ]
                 , li [] [ text ("Number of times D has been pressed: " ++ (toString model.numD)) ]
                 , li [] [ text <| "Number of ticks: " ++ (toString model.numDelta) ]
                 , li [] [ text <| "Last dt: " ++ (toString model.lastDelta) ]
                 ]
         ]


ticker = fps 30


-- Signal is more then 30 fps if keyboard inputs are coming
-- in. Keyboard inputs are only registered when they change, so they
-- don't cause the update function to register ongoing input.
kindaWorkingInputs =
  merge (map Tick ticker) (map Wasd Keyboard.wasd)


-- Sampling the keyboard will only work if the keyboard is pressed and
-- ticker fires at exactly the same time?
brokenInputs =
  merge
    (map Tick ticker)
      (sampleOn ticker <| map Wasd Keyboard.wasd)


-- Same as above, sampling the merged signal is extremely unlikely
-- to sample a keypress instead of a tick
brokenInputs2 =
  sampleOn ticker <| merge (map Tick ticker) (map Wasd Keyboard.wasd)


type alias Inputs =
    { x : Int
    , y : Int
    , dt : Float
    }


-- Keyboard and ticker inputs are merged togeter into a record, which
-- holds the last value of the signal until it is sampled. This
-- essentially repeats the keypress on every tick as far as the update
-- function is concerned.
inputs = map Input <| sampleOn ticker <|
         map3 Inputs (map .x Keyboard.wasd) (map .y Keyboard.wasd) ticker


main = map view <| foldp update init inputs
