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


initialBoundingBox : BoundingBox
initialBoundingBox =
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


add : Position -> Position -> BoundingBox -> Position
add position { x, y } boundingBox =
    { position
        | x = clamp boundingBox.topLeft.x boundingBox.bottomRight.x (position.x + x)
        , y = clamp boundingBox.topLeft.y boundingBox.bottomRight.y (position.y - y)
    }


touches : Position -> BoundingBox -> (Position -> Int) -> Bool
touches position boundingBox axis =
    axis position == axis boundingBox.topLeft || axis position == axis boundingBox.bottomRight


scalarMultiplication : number -> Position -> Position
scalarMultiplication velocity { x, y } =
    { x = x * velocity, y = y * velocity }
