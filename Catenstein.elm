module Main exposing (..)

import Collage exposing (..)
import Element exposing (Element, flow, right)
import Color
import Time
import Keyboard
import AnimationFrame
import Math.Vector2 exposing (..)
import VectorUtils exposing (..)
import Html exposing (program)


type Tile
    = BlueFloor
    | RedFloor


type alias Texture =
    String


type Material
    = Painted Color.Color
    | Wallpapered Texture


type Edge
    = Wall Material
    | Air


type alias WorldConstants =
    { floorTiles : List Tile
    , horizontalEdges : List Edge -- y is constant
    , verticalEdges : List Edge -- x is constant
    , size : ( Int, Int )
    }


type alias WorldViewConstants =
    { tileHeight : Float
    , tileWidth : Float
    , wallHeight : Float
    }


type alias PlayerViewConstants =
    { fov : Float
    , height : Float
    , planeWidth : Float
    , planeHeight : Float
    , distanceFromPlaneToPlayer : Float
    }


yWall : Edge
yWall =
    Wall (Painted Color.yellow)


rWall : Edge
rWall =
    Wall (Painted Color.red)


world : WorldConstants
world =
    { size = ( 5, 5 )

    -- Remember with these that +y is up and +x is to the right when this is drawn,
    -- so the textual representation of the list is "upside down".
    , floorTiles =
        [ BlueFloor
        , BlueFloor
        , BlueFloor
        , BlueFloor
        , BlueFloor
        , BlueFloor
        , RedFloor
        , RedFloor
        , RedFloor
        , BlueFloor
        , BlueFloor
        , RedFloor
        , RedFloor
        , RedFloor
        , BlueFloor
        , BlueFloor
        , RedFloor
        , RedFloor
        , RedFloor
        , BlueFloor
        , BlueFloor
        , BlueFloor
        , RedFloor
        , BlueFloor
        , BlueFloor
        ]
    , horizontalEdges =
        [ yWall
        , yWall
        , yWall
        , yWall
        , yWall -- y is constant
        , Air
        , Air
        , Air
        , Air
        , Air
        , Air
        , Air
        , Air
        , Air
        , Air
        , Air
        , Air
        , Air
        , Air
        , Air
        , Air
        , Air
        , Air
        , Air
        , Air
        , yWall
        , yWall
        , yWall
        , yWall
        , yWall
        ]
    , verticalEdges =
        [ rWall
        , Air
        , Air
        , Air
        , Air
        , rWall -- x is constant
        , rWall
        , Air
        , Air
        , Air
        , Air
        , rWall
        , rWall
        , Air
        , Air
        , Air
        , Air
        , rWall
        , rWall
        , Air
        , Air
        , Air
        , Air
        , rWall
        , rWall
        , Air
        , Air
        , Air
        , Air
        , rWall
        ]
    }


worldView : WorldViewConstants
worldView =
    { wallHeight = 64
    , tileWidth = 64
    , tileHeight = 64
    }


playerView : PlayerViewConstants
playerView =
    let
        fov =
            (degrees 60.0)

        ppX =
            320.0

        ppY =
            200.0

        ppcX =
            ppX / 2.0

        ppcY =
            ppY / 2.0

        d =
            ppcX / (tan fov)
    in
        { height = 32
        , fov = fov
        , planeWidth = ppX
        , planeHeight = ppY
        , distanceFromPlaneToPlayer = d
        }


type alias Model =
    { position : Vec2
    , lookAngle : Float
    , speed : Float
    }


type Input
    = Tick Float
    | Wasd Keyboard.KeyCode


model : Model
model =
    { position = vec2 0.0 0.0
    , lookAngle = (degrees 90.0)
    , speed = 1.0
    }


main =
    program
        { init = ( model, Cmd.none )
        , view = view
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ AnimationFrame.times Tick
                    , Keyboard.presses Wasd
                    ]
        }


toWasd : Keyboard.KeyCode -> { x : Int, y : Int }
toWasd code =
    case code of
        87 ->
            { x = 0, y = 1 }

        83 ->
            { x = 0, y = -1 }

        65 ->
            { x = -1, y = 0 }

        68 ->
            { x = 1, y = 0 }

        _ ->
            { x = 0, y = 0 }


update : Input -> Model -> ( Model, Cmd Input )
update i m =
    case i of
        Wasd keycode ->
            ( movement (toWasd keycode) m, Cmd.none )

        Tick dt ->
            ( m, Cmd.none )


movement : { x : Int, y : Int } -> Model -> Model
movement keys m =
    let
        newLookAngle =
            m.lookAngle - (degrees (toFloat keys.x))

        velocity =
            Math.Vector2.scale m.speed (vrotate (vec2 1.0 0.0) newLookAngle)

        newPosition =
            add m.position (Math.Vector2.scale (toFloat keys.y) velocity)
    in
        { m
            | position = newPosition
            , lookAngle = newLookAngle
        }


view : Model -> Html.Html Input
view m =
    flow right
        [ collage 640 480 (viewMap ++ [ viewPlayer m ])
        , viewScene m
        ]
        |> Element.toHtml


playerIcon : Form
playerIcon =
    traced (solid Color.black) (path [ ( -10, 10 ), ( 0.0, 0.0 ), ( -10.0, -10.0 ) ])


viewPlayer : Model -> Form
viewPlayer m =
    playerIcon
        |> move ( getX m.position, getY m.position )
        |> rotate m.lookAngle


tileFillColor : Tile -> Color.Color
tileFillColor t =
    case t of
        BlueFloor ->
            Color.blue

        RedFloor ->
            Color.red


drawTile : Tile -> Form
drawTile t =
    rect (worldView.tileWidth - 1) (worldView.tileHeight - 1)
        |> filled (tileFillColor t)


positionTile : Int -> Form -> Form
positionTile p f =
    let
        ( wx, wy ) =
            world.size

        halfTile =
            worldView.tileWidth / 2.0

        centreOffset =
            (((toFloat wx) * worldView.tileWidth) / 2.0 - halfTile)

        x =
            (toFloat (p % wx)) * worldView.tileWidth

        y =
            (toFloat (p // wx)) * worldView.tileHeight
    in
        move ( x - centreOffset, y - centreOffset ) f


drawEdge : Edge -> Form
drawEdge e =
    case e of
        Air ->
            filled Color.white (rect 1 worldView.tileHeight)

        Wall m ->
            drawWallEdge m


drawWallEdge : Material -> Form
drawWallEdge m =
    case m of
        Painted c ->
            filled c (rect 1 worldView.tileHeight)

        -- TODO: Texturing
        Wallpapered t ->
            filled Color.black (rect 1 worldView.tileHeight)


drawHorizontalEdge : Int -> Edge -> Form
drawHorizontalEdge p e =
    let
        ( numTilesX, numTilesY ) =
            world.size

        numEdgesX =
            toFloat (numTilesX + 1)

        numEdgesY =
            toFloat (numTilesY + 1)

        halfWorldWidth =
            (worldView.tileWidth * (toFloat numTilesX) / 2.0)

        halfWorldHeight =
            (worldView.tileHeight * (toFloat numTilesY) / 2.0)

        halfTileWidth =
            worldView.tileWidth / 2.0

        halfTileHeight =
            worldView.tileHeight / 2.0

        x =
            (toFloat (p % numTilesX)) * worldView.tileWidth - halfWorldWidth + halfTileWidth

        y =
            (toFloat (p // numTilesX)) * worldView.tileHeight - halfWorldHeight

        f =
            rotate (degrees 90) (drawEdge e)
    in
        move ( x, y ) f


drawVerticalEdge : Int -> Edge -> Form
drawVerticalEdge p e =
    let
        ( numTilesX, numTilesY ) =
            world.size

        numEdgesX =
            toFloat (numTilesX + 1)

        numEdgesY =
            toFloat (numTilesY + 1)

        halfWorldWidth =
            (worldView.tileWidth * (toFloat numTilesX) / 2.0)

        halfWorldHeight =
            (worldView.tileHeight * (toFloat numTilesY) / 2.0)

        halfTileWidth =
            worldView.tileWidth / 2.0

        halfTileHeight =
            worldView.tileHeight / 2.0

        x =
            (toFloat (p % (round numEdgesX))) * worldView.tileWidth - halfWorldWidth

        y =
            (toFloat (p // (round numEdgesX))) * worldView.tileHeight - halfWorldHeight + halfTileHeight

        f =
            drawEdge e
    in
        move ( x, y ) f


viewMap : List Form
viewMap =
    List.indexedMap (\p t -> positionTile p (drawTile t)) world.floorTiles
        ++ List.indexedMap drawHorizontalEdge world.horizontalEdges
        ++ List.indexedMap drawVerticalEdge world.verticalEdges


viewScene : Model -> Element
viewScene m =
    let
        halfFov =
            playerView.fov / 2

        start =
            m.lookAngle + halfFov

        end =
            m.lookAngle - halfFov

        increment =
            -1 * playerView.fov / playerView.planeWidth

        angles =
            buildAngleList start end increment

        distancesToWall =
            List.map (raycast m.position m.lookAngle) angles

        wallSliceHeights =
            List.map (\d -> 64 / d * 277) distancesToWall

        wallSlices =
            List.map (\h -> (filled Color.blue (rect 1 h))) wallSliceHeights

        shiftedWallSlices =
            List.indexedMap (\x s -> moveX (toFloat x - 160) s) wallSlices
    in
        collage 320 200 ([ filled Color.black (rect 320 200) ] ++ shiftedWallSlices)


buildAngleList : Float -> Float -> Float -> List Float
buildAngleList start end increment =
    if start < end then
        []
    else
        start :: (buildAngleList (start + increment) end increment)


raycast : Vec2 -> Float -> Float -> Float
raycast position lookAngle angle =
    let
        -- Check outer boundaries only for now
        -- This is hard coded to 5x5 with 64x64 unit tiles
        lookingUp =
            angle > 0.0 && angle < (degrees 180)

        lookingLeft =
            angle > (degrees 90) && angle < (degrees 270)

        horzGridLines =
            if lookingUp then
                [ 160 ]
            else
                [ -160 ]

        vertGridLines =
            if lookingLeft then
                [ -160 ]
            else
                [ 160 ]

        horzDistances =
            List.map (\y -> (y - (getY position)) / sin angle) horzGridLines

        vertDistances =
            List.map (\x -> (x - (getX position)) / cos angle) vertGridLines

        -- This distance will look "fisheye" and needs correction
        fisheyeDistance =
            Maybe.withDefault 0.0 (List.minimum (horzDistances ++ vertDistances))

        beta =
            if angle > lookAngle then
                angle - lookAngle
            else
                lookAngle - angle

        correctedDistance =
            fisheyeDistance * (cos beta)
    in
        correctedDistance
