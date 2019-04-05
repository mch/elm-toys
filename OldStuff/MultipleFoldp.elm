module Main exposing (..)

import Signal exposing (..)
import Mouse
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Window
import Time exposing (Time, fps, inMilliseconds)


main =
    map show o


delay =
    fps 30



-- Trying out multiple foldp's


totalTime =
    foldp (\dt s -> s + dt) 0 delay


clicks =
    foldp (\x s -> s + 1) 0 Mouse.clicks


o =
    map3 (,,) totalTime Mouse.position clicks


signals =
    merge
