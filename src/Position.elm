module Position exposing (..)


type alias Position =
    { x : Int
    , y : Int
    }


fromXY : Int -> Int -> Position
fromXY x y =
    Position x y


initial : Position
initial =
    fromXY 0 0


scalarMultiplication : number -> Position -> Position
scalarMultiplication velocity { x, y } =
    { x = x * velocity, y = y * velocity }
