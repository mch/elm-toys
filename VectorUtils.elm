module VectorUtils where
import Math.Vector2 exposing (..)

vrotate : Vec2 -> Float -> Vec2
vrotate v a =
  let
    x = (getX v) * cos a - (getY v) * sin a
    y = (getX v) * sin a + (getY v) * sin a
  in    
    vec2 x y
