module Position exposing (..)

import List exposing (..)
import Helpers exposing (..)


type alias Position =
    { x : Int
    , y : Int
    }


type alias BoundingBox =
    { topLeft : Position
    , bottomRight : Position
    }


initalBoundingBox =
    BoundingBox (fromXY 0 0) (fromXY 1024 768)


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


add : Position -> Position -> BoundingBox -> Position
add position { x, y } boundingBox =
    { position
        | x = normalize boundingBox.bottomRight.x (position.x + x)
        , y = normalize boundingBox.bottomRight.y (position.y - y)
    }


scalarMultiplication : number -> Position -> Position
scalarMultiplication velocity { x, y } =
    { x = x * velocity, y = y * velocity }
