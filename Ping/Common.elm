module Common exposing (..)

{-| The position of an object in game space, which is the same as screen space
right now. This should perhaps be a component... Rather than position, a
component grouping translation, rotation, and scale might be more appropriate.
-}


type alias Position =
    ( Float, Float )
