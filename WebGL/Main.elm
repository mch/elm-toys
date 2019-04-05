module Triangle exposing (main)

{-
   Rotating triangle, that is a "hello world" of the WebGL

   This is a modified version of this
   https://github.com/elm-explorations/webgl/blob/master/examples/triangle.elm
-}

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Debug
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import WebGL exposing (Mesh, Shader)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Json.Decode exposing (Value)


main : Program Value Float Float
main =
    Browser.element
        { init = \_ -> ( 0, Cmd.none )
        , view = view
        , subscriptions = (\_ -> onAnimationFrameDelta Basics.identity)
        , update = (\elapsed currentTime -> ( elapsed + currentTime, Cmd.none ))
        }


view : Float -> Html msg
view t =
    WebGL.toHtml
        [ width 400
        , height 400
        , style "display" "block"
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            circle
            { perspective = perspective (t / 1000) }
        ]


perspective : Float -> Mat4
perspective t =
    let
        fov_y =
            60

        aspect_ratio =
            1

        z_near =
            0.01

        z_far =
            100

        eye =
            --(vec3 1 (4 * cos t) (4 * sin t))
            (vec3 0 1 4)

        center =
            (vec3 0 0 0)

        up =
            (vec3 0 1 0)
    in
        Mat4.mul
            --(Mat4.makePerspective fov_y aspect_ratio z_near z_far)
            --(Mat4.makeLookAt eye center up)
            (Mat4.makeOrtho -1 1 -1 1 1 -1)
            (Mat4.makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0))



-- Mesh


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


mesh : Mesh Vertex
mesh =
    WebGL.triangles
        [ ( Vertex (vec3 0 0 0) (vec3 1 0 0)
          , Vertex (vec3 1 1 0) (vec3 0 1 0)
          , Vertex (vec3 1 -1 0) (vec3 0 0 1)
          )
        ]


circle : Mesh Vertex
circle =
    List.range 0 45
        |> List.map (\a -> degrees ((toFloat a) * 8))
        |> List.map (\a -> Vertex (vec3 (cos a) (sin a) 0) (vec3 (a / pi) 0 0))
        |> List.append [ Vertex (vec3 0 0 0) (vec3 0 0 0) ]
        |> WebGL.triangleFan



-- Shaders


type alias Uniforms =
    { perspective : Mat4 }


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        varying vec3 vcolor;

        void main () {
            gl_Position = perspective * vec4(position, 1.0);
            vcolor = color;
        }

    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|

        precision mediump float;
        varying vec3 vcolor;

        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }

    |]
