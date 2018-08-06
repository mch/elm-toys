module Main exposing (..)

import AnimationFrame exposing (..)
import Dict
import Html exposing (..)
import Time exposing (..)


type alias Tween a =
    { start : a
    , end : a
    , startTime : Time
    , duration : Time
    }


type alias Model =
    { t : Time
    , dt : Time
    , current : Float
    , tween : Maybe (Tween Float)
    }


init =
    ( Model 0 0 0 Maybe.Nothing, Cmd.none )


viewTween tween =
    case tween of
        Just tween ->
            [ p [] [ text ("Start: " ++ (toString tween.start)) ]
            , p [] [ text ("End: " ++ (toString tween.end)) ]
            , p [] [ text ("Start time: " ++ (toString tween.startTime)) ]
            , p [] [ text ("Duration: " ++ (toString tween.duration)) ]
            ]

        Nothing ->
            []


view model =
    div []
        (List.append
            [ h1 [] [ text "Tween Test" ]
            , p [] [ text ("Time: " ++ (toString model.t)) ]
            , p [] [ text ("dt: " ++ (toString model.dt)) ]
            , p [] [ text ("Current: " ++ (toString model.current)) ]
            ]
            (viewTween model.tween)
        )


type Msg
    = Tick Time


lerp tween t =
    let
        x =
            t - tween.startTime

        b =
            tween.start

        m =
            (tween.end - tween.start) / tween.duration

        y =
            if t > tween.startTime + tween.duration then
                tween.end
                -- remove the tween from processing...
            else
                x * m + b
    in
        y


handleTick : Time -> Model -> Model
handleTick t model =
    let
        dt =
            t - model.t

        tween : Tween Float
        tween =
            Maybe.withDefault (Tween 0 100 t 5000) model.tween

        current =
            lerp tween t
    in
        { model | t = t, dt = dt, tween = Maybe.Just tween, current = current }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t ->
            ( handleTick t model, Cmd.none )


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> AnimationFrame.times Tick
        }
