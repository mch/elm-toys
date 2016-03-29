import Keyboard
import Html exposing (..)
import Char


type alias Model =
  { numSpaces : Int
  , numEnters : Int
  }


init = { numSpaces = 0, numEnters = 0 }


type Action = Space Bool | Enter Bool


update action model =
  case action of
    Space b -> { model | numSpaces = model.numSpaces + (if b then 1 else 0) }
    Enter b -> { model | numEnters = model.numEnters + (if b then 1 else 0) }


view model =
  div [] [ p [] [text ("Num spaces: " ++ (toString model.numSpaces))]
         , p [] [text ("Num enters: " ++ (toString model.numEnters))]]


spaceSignal = Keyboard.space
enterSignal = Keyboard.enter
shiftSignal = Keyboard.shift
              

-- Without sampling, both inputs go through as you would expect.
-- Merging signals: if an update comes from multiple signals at the
-- same time, then only the first gets through and the rest are
-- discarded.
inputs = Signal.merge
  (Signal.map Space spaceSignal)
  (Signal.map Enter enterSignal)
--main = Signal.map view (Signal.foldp update init inputs)


-- When enter is sampled this way, the signal only gets through when
-- enter and space are pressed at basically the same time. Space is
-- only used for sampling, so it never gets through.
sampledInputs = Signal.sampleOn spaceSignal (Signal.map Enter enterSignal)
--main = Signal.map view (Signal.foldp update init sampledInputs)


-- When inputs are sampled like this, enter never get's through, but
-- space does, likely because of the 
sampledInputs2 = Signal.sampleOn spaceSignal inputs
--main = Signal.map view (Signal.foldp update init sampledInputs2)


-- When the sampling occurs on shift, toggling shift causes a sample
-- to leak through. If space or enter is held down, and shift is
-- toggled, then the counter will increment. The merge is what really
-- is messing things up here.
sampledInputs3 = Signal.sampleOn shiftSignal inputs
main = Signal.map view (Signal.foldp update init sampledInputs3)


-- So, how does sampleOn work? It seems to retain the previous value
-- of a signal for a short amount of time, but if the signal is not
-- "firing" when it is sampled, nothing goes through.



