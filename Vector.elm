module Vector where

type alias Vector2 =
  { x : Float
  , y : Float
  }


vec2 : Float -> Float -> Vector2
vec2 x y = { x = x, y = y }


add : Vector2 -> Vector2 -> Vector2
add a b =
  { x = a.x + b.x
  , y = a.y + b.y
  }


addX : Vector2 -> Float -> Vector2
addX a x =
  { a | x = a.x + x }


addY : Vector2 -> Float -> Vector2
addY a y =
  { a | y = a.y + y }
  

vrotate : Vector2 -> Float -> Vector2
vrotate v a =
  { v | x = v.x * cos a - v.y * sin a
      , y = v.x * sin a + v.y * sin a }


mul : Vector2 -> Float -> Vector2
mul v m = { v | x = v.x * m, y = v.y * m }
          
  
