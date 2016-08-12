module Position exposing (..)

import List exposing (..)


type alias Position =
    { x : Int
    , y : Int
    }


spriteSize : number
spriteSize =
    128


halfSpriteSize : Int
halfSpriteSize =
    spriteSize / 2 |> round


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


add : Position -> Position -> Position
add position { x, y } =
    { position
        | x = normalize 1000 (position.x + x)
        , y = normalize 768 (position.y - y)
    }


scalarMultiplication : number -> Position -> Position
scalarMultiplication velocity { x, y } =
    { x = x * velocity, y = y * velocity }
