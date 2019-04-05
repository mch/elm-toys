module Main exposing (..)

import Keyboard
import Html exposing (..)
import Time


type alias Model =
    { currentValue : Bool
    , numUpdates : Int
    }


init =
    { currentValue = False, numUpdates = 0 }


update a m =
    { m
        | currentValue = a
        , numUpdates = m.numUpdates + 1
    }


view m =
    div []
        [ p [] [ text ("Num updates: " ++ (toString m.numUpdates)) ]
        , p [] [ text ("Current value: " ++ toString m.currentValue) ]
        ]


inputs =
    Keyboard.space



-- no sampling
--main = Signal.map view <| Signal.foldp update init inputs
-- sampling on enter
--main = Signal.map view <| Signal.foldp update init (Signal.sampleOn (Keyboard.enter) inputs)
-- sampling on a timer


timer =
    Time.fps 2



--main = Signal.map view <| Signal.foldp update init (Signal.sampleOn timer inputs)
-- And, now with a merge that will mess everything up. Because the
-- timer update is mapped to always return a boolean of False, and we
-- are sampling on the timer as well, the update from the
-- Keyboard.space signal never gets through.
--main = Signal.map view <| Signal.foldp update init (Signal.sampleOn timer (Signal.merge (Signal.map (\x -> True) timer) inputs))
-- A different way to merge stuff and sample it, using records


type alias Inputs =
    { dt : Float, space : Bool }


recordedInputs =
    Signal.map2 (\dt space -> { dt = dt, space = space }) timer inputs



-- Discarding the timer. This allows multiple keypresses through on
-- each timer tick though, which can cause problems for games.
--main = Signal.map view <| Signal.foldp (\x -> update x.space) init recordedInputs
-- Sampling to the rescue. Mashing the space bar doesn't result in
-- multiple updates within the period of the timer.


main =
    Signal.map view <| Signal.foldp (\x -> update x.space) init (Signal.sampleOn timer recordedInputs)



-- This is why Signals always have to have a default value. They may
-- be sampled before they have ever been updated.
