module Position exposing (..)

import List exposing (..)
import Helpers exposing (..)


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


collision : Position -> Position -> Bool
collision a b =
    all (lateralCollision a b) [ .x, .y ]


lateralCollision : Position -> Position -> (Position -> Int) -> Bool
lateralCollision a b axis =
    abs (axis a - axis b) < halfSpriteSize


normalize : number -> number -> number
normalize =
    clamp 0


max : Position
max =
    { x = 1024
    , y = 768
    }


add : Position -> Position -> Position
add position { x, y } =
    { position
        | x = normalize max.x (position.x + x)
        , y = normalize max.y (position.y - y)
    }


scalarMultiplication : number -> Position -> Position
scalarMultiplication velocity { x, y } =
    { x = x * velocity, y = y * velocity }
