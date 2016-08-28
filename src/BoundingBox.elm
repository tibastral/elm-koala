module BoundingBox exposing (..)

import Position exposing (Position)


type alias BoundingBox =
    { topLeft : Position
    , bottomRight : Position
    }


initialBoundingBox : BoundingBox
initialBoundingBox =
    fromTwoPositions (Position.fromXY 0 0) (Position.fromXY 1024 768)


touches : Position -> BoundingBox -> (Position -> Int) -> Bool
touches position boundingBox axis =
    axis position == axis boundingBox.topLeft || axis position == axis boundingBox.bottomRight


width boundingBox =
    boundingBox.bottomRight.x - boundingBox.topLeft.x


height boundingBox =
    boundingBox.bottomRight.y - boundingBox.topLeft.y



-- we should integrate that
-- https://www.toptal.com/game/video-game-physics-part-i-an-introduction-to-rigid-body-dynamics


collision : BoundingBox -> BoundingBox -> Bool
collision a b =
    (abs (a.topLeft.x - b.topLeft.x) * 2 < (width a) + (width b))
        && (abs (a.topLeft.y - b.topLeft.y) * 2 < (height a) + (height b))



-- (a.topLeft.x |> between b.topLeft.x b.bottomRight.x) && (a.topLeft.y |> between b.topLeft.y b.topLeft.y)


between begin end number =
    number >= begin && number <= end


fromTwoPositions : Position -> Position -> BoundingBox
fromTwoPositions a b =
    BoundingBox a b
