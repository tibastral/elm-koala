module Vector exposing (..)


type alias Vector =
    { x : Int
    , y : Int
    }


fromXY : Int -> Int -> Vector
fromXY x y =
    Vector x y


initial : Vector
initial =
    fromXY 0 0


scalarMultiplication : Int -> Vector -> Vector
scalarMultiplication velocity { x, y } =
    { x = x * velocity, y = y * velocity }
