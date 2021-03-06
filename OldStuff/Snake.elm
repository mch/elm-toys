module Snake exposing (Model, Input, view, update, init, inputs)

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color
import Signal exposing (Address, message)
import Time
import Keyboard
import Debug
import Random
import Text


type Input
    = Keyboard { x : Int, y : Int }
    | Tick Time.Time


canvasSize =
    { width = 800, height = 600 }


topStatsSize =
    { width = 800, height = 20, x = 0, y = 290 }


boardSize =
    { width = 800
    , height = canvasSize.height - topStatsSize.height
    , x = 0
    , y = -topStatsSize.height
    }


borderThickness =
    10


snakeSize =
    { width = 10, height = 10 }


snakeFence =
    { maxX = 38 --(canvasSize.width - borderThickness) / snakeSize.width / 2
    , minX = -38 -- (canvasSize.width - borderThickness) / snakeSize.width / 2
    , maxY = 26 --(canvasSize.height - borderThickness - topStatsSize.height) / snakeSize.width / 2
    , minY = -27 -- -((canvasSize.height - borderThickness) / snakeSize.width / 2)
    }


type alias Point =
    { x : Float, y : Float }


type alias Vector =
    { x : Float, y : Float }


type State
    = Ready
    | Set
    | Go
    | Playing
    | Paused
    | Death
    | GameOver


type alias Model =
    { state : State
    , score : Int
    , lives : Int
    , speed : Float
    , timeSinceLastMove : Time.Time
    , timeSinceLastFood : Time.Time
    , timeToStateChng : Time.Time
    , maxFood : Int
    , newLength : Int
    , direction : Vector
    , seed : Random.Seed
    , foodX : Random.Generator Float
    , foodY : Random.Generator Float
    , food : List Point
    , snake : List Point
    }


init : Model
init =
    { state = Ready
    , score = 0
    , lives = 3
    , speed = 8 -- blocks per second
    , timeSinceLastMove = 0
    , timeSinceLastFood = 0
    , timeToStateChng = Time.second
    , maxFood = 10
    , newLength = 2
    , direction = { x = 0, y = 1 }
    , seed = Random.initialSeed 0
    , foodX = Random.float snakeFence.minX snakeFence.maxX
    , foodY = Random.float snakeFence.minY snakeFence.maxY
    , food = []
    , snake = []
    }


delta =
    Time.fps 30



-- Taking inspiration from the Pong example, sample only on the delta
-- to avoid extra inputs between steps (which would allow the snake
-- to reverse on itself for instance). Need to change how the inputs
-- are gathered together though.
-- arrows = Signal.sampleOn delta (Keyboard.arrows)


arrows =
    Keyboard.arrows


inputs =
    [ Signal.map (\tick -> Tick tick) delta
    , Signal.map (\direction -> Keyboard direction) arrows
    ]


update : Input -> Model -> Model
update i m =
    Debug.watch "Snake model" (updateModel i m)


updateModel i m =
    case i of
        Keyboard d ->
            updateFromInput d m

        Tick t ->
            updateForState t m


updateForState t m =
    case m.state of
        Ready ->
            stateTransition t Set Time.second m

        Set ->
            stateTransition t Go Time.second m

        Go ->
            stateTransition t Playing Time.second m

        Playing ->
            moveSnake t m
                |> snakeEats
                |> addFood t
                |> checkForDeath

        Paused ->
            m

        Death ->
            stateTransition t (deadOrAlive m) Time.second m

        GameOver ->
            stateTransition t Ready Time.second m


deadOrAlive : Model -> State
deadOrAlive m =
    if m.lives <= 0 then
        GameOver
    else
        Playing


stateTransition : Time.Time -> State -> Time.Time -> Model -> Model
stateTransition t nextState nextStateDuration m =
    let
        timeRemaining =
            m.timeToStateChng - t
    in
        if timeRemaining <= 0 then
            { m | state = nextState, timeToStateChng = nextStateDuration }
        else
            { m | timeToStateChng = timeRemaining }


updateFromInput : { x : Int, y : Int } -> Model -> Model
updateFromInput d m =
    let
        direction =
            { x = toFloat d.x, y = toFloat d.y }

        dotProduct =
            m.direction.x * direction.x + m.direction.y * direction.y
    in
        if (d.x /= 0 || d.y /= 0) && dotProduct == 0 then
            { m | direction = direction }
        else
            m


moveSnake : Time.Time -> Model -> Model
moveSnake t m =
    let
        timeSinceLastMove =
            t + m.timeSinceLastMove

        m_ =
            { m | timeSinceLastMove = timeSinceLastMove }
    in
        if Time.inSeconds timeSinceLastMove > 1 / m.speed then
            let
                m__ =
                    stepSnakeAhead m_
            in
                { m__ | timeSinceLastMove = 0 }
        else
            m_


stepSnakeAhead : Model -> Model
stepSnakeAhead m =
    let
        firstPoint =
            Maybe.withDefault { x = 0, y = 0 } (List.head m.snake)

        newPoint =
            moveSnakePoint m.direction firstPoint

        grow =
            m.newLength > List.length m.snake
    in
        if grow then
            { m | snake = newPoint :: m.snake }
        else
            { m | snake = newPoint :: List.take (List.length m.snake - 1) m.snake }


moveSnakePoint : { x : Float, y : Float } -> { x : Float, y : Float } -> { x : Float, y : Float }
moveSnakePoint d p =
    { x = wrapAround (p.x + d.x) snakeFence.maxX snakeFence.minX
    , y = wrapAround (p.y + d.y) snakeFence.maxY snakeFence.minY
    }


wrapAround : Float -> Float -> Float -> Float
wrapAround x max min =
    if x > max then
        min
    else if x < min then
        max
    else
        x


addFood : Time.Time -> Model -> Model
addFood t m =
    let
        ( newFoodX, seed1 ) =
            Random.generate m.foodX m.seed

        ( newFoodY, seed2 ) =
            Random.generate m.foodY seed1

        newFood =
            { x = toFloat (floor newFoodX), y = toFloat (floor newFoodY) }

        updatedModel =
            { m
                | timeSinceLastFood = m.timeSinceLastFood + t
                , seed = seed2
            }

        -- TODO ensure the new food does not overlap anything else on the
        -- board already, including the snake.
    in
        if List.length m.food < m.maxFood && (Time.inSeconds m.timeSinceLastFood) > 2 then
            { updatedModel
                | food = newFood :: updatedModel.food
                , timeSinceLastFood = 0
            }
        else
            updatedModel


snakeEats : Model -> Model
snakeEats m =
    let
        snakeHead =
            Maybe.withDefault { x = 0, y = 0 } (List.head m.snake)

        eatenFood =
            List.filter (\f -> snakeHead == f) m.food
    in
        if List.length eatenFood > 0 then
            { m
                | food = List.filter (\f -> snakeHead /= f) m.food
                , score = m.score + 1000
                , newLength = m.newLength + 2
            }
        else
            m


checkForDeath : Model -> Model
checkForDeath m =
    let
        snakeHead =
            Maybe.withDefault { x = 0, y = 0 } (List.head m.snake)

        snakeTail =
            Maybe.withDefault [] (List.tail m.snake)

        collidedWithSelf =
            List.length (List.filter (\s -> snakeHead == s) snakeTail) > 0
    in
        if collidedWithSelf then
            { m
                | state = Death
                , lives = m.lives - 1
                , food = []
                , snake = []
                , newLength = 2
                , direction = { x = 0, y = 1 }
            }
        else
            m


view : Address Input -> Model -> Element
view a m =
    case m.state of
        Ready ->
            drawText "Ready!"

        Set ->
            drawText "Set!"

        Go ->
            drawText "Go!"

        Playing ->
            collage (round canvasSize.width) (round canvasSize.height) (forms m)

        Paused ->
            drawText "Paused!"

        Death ->
            drawText "You Died!"

        GameOver ->
            drawText "Game Over!"


drawText : String -> Element
drawText s =
    collage (round canvasSize.width) (round canvasSize.height) [ text (Text.fromString s) ]


forms : Model -> List Form
forms m =
    [ board
    , border
    , statusBar
    , viewScore m.score
    , viewSnake m.snake Color.red
    , viewSnake m.food Color.white
    ]


board : Form
board =
    rect canvasSize.width canvasSize.height
        |> filled Color.yellow


statusBar : Form
statusBar =
    move ( topStatsSize.x, topStatsSize.y ) (filled Color.blue (rect topStatsSize.width topStatsSize.height))


border : Form
border =
    let
        initialLineStyle =
            solid Color.orange

        style =
            { initialLineStyle | width = 10 }

        top =
            { w = boardSize.width, h = borderThickness, x = 0, y = canvasSize.height / 2 - topStatsSize.height - borderThickness / 2 }

        bottom =
            { w = boardSize.width, h = borderThickness, x = 0, y = -(canvasSize.height / 2 - borderThickness / 2) }

        left =
            { w = borderThickness, h = boardSize.height, x = -(canvasSize.width / 2 - borderThickness / 2), y = -topStatsSize.height / 2 }

        right =
            { w = borderThickness, h = boardSize.height, x = canvasSize.width / 2 - borderThickness / 2, y = -topStatsSize.height / 2 }

        borders =
            [ top, bottom, left, right ]

        drawBorder b =
            move ( b.x, b.y ) (filled Color.orange (rect b.w b.h))
    in
        group (List.map drawBorder borders)


viewSnake : List Point -> Color.Color -> Form
viewSnake s c =
    let
        collagePoints =
            List.map
                (\p ->
                    { p
                        | x = p.x * snakeSize.width
                        , y = p.y * snakeSize.height
                    }
                )
                s

        drawPoint p =
            move ( p.x, p.y ) (filled c (rect snakeSize.width snakeSize.height))
    in
        group (List.map drawPoint collagePoints)


viewScore : Int -> Form
viewScore s =
    let
        titleText =
            Text.fromString "Score: "

        scoreText =
            Text.fromString (toString s)
    in
        Text.append titleText scoreText
            |> Text.color Color.white
            |> text
            |> move ( 300, 292 )
