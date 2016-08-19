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



-- collision : Position -> Position -> Float -> Bool
-- collision a b spriteSize =
--     all (lateralCollision a b spriteSize) [ .x, .y ]
--
--
-- lateralCollision : Position -> Position -> Float -> (Position -> Int) -> Bool
-- lateralCollision a b spriteSize axis =
--     abs (axis a - axis b) < (spriteSize / 2 |> round)


scalarMultiplication : number -> Position -> Position
scalarMultiplication velocity { x, y } =
    { x = x * velocity, y = y * velocity }
