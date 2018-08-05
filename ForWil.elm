-- This was done for Wil based on some stuff in the Usborne book
-- Lift-the-flap Computers and Coding.
--
-- Except for the animation. That's all me.


module Main exposing (..)

import Graphics.Collage exposing (..)
import Color exposing (black, white)
import StartApp exposing (start)
import Time exposing (fps, Time)
import Effects
import Html exposing (fromElement)


xSize =
    10


pixelSize =
    20


picture =
    [ 0
    , 1
    , 0
    , 1
    , 0
    , 0
    , 1
    , 0
    , 0
    , 0
    , 1
    , 0
    , 0
    , 1
    , 0
    , 0
    , 1
    , 0
    , 0
    , 0
    , 0
    , 0
    , 1
    , 1
    , 1
    , 1
    , 1
    , 1
    , 0
    , 0
    , 0
    , 1
    , 0
    , 1
    , 0
    , 1
    , 0
    , 1
    , 0
    , 1
    , 0
    , 1
    , 0
    , 1
    , 0
    , 1
    , 0
    , 1
    , 0
    , 1
    ]


toBox : Int -> Int -> Form
toBox index i =
    let
        box =
            rect pixelSize pixelSize

        xoffset =
            pixelSize * (index % 10)

        yoffset =
            -pixelSize * (index // 10)

        color =
            if i == 0 then
                black
            else
                white

        form =
            filled color box
    in
        move ( toFloat xoffset, toFloat yoffset ) form


rotate : List a -> List a
rotate l =
    let
        last =
            List.drop (List.length l - 1) l
    in
        List.take (List.length l) (List.append last l)



-- Boiler plate...


type alias Model =
    { picture : List Int }


type Action
    = Tick Time


init =
    ( { picture = picture }, Effects.none )


update action m =
    ( { m | picture = rotate m.picture }, Effects.none )


view addr m =
    let
        forms =
            List.indexedMap toBox m.picture
    in
        collage 800 600 forms |> fromElement


app =
    start { init = init, update = update, view = view, inputs = [ (fps 1) ] }


main =
    app.html


port runner : Signal (Task Effects.Never ())
port runner =
    app.tasks
