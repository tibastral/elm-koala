module BoundingBox exposing (..)

import Vector exposing (Vector)


type alias BoundingBox =
    { topLeft : Vector
    , bottomRight : Vector
    }


initialBoundingBox : BoundingBox
initialBoundingBox =
    fromTwoVectors (Vector.fromXY 0 0) (Vector.fromXY 1024 768)


touches : Vector -> BoundingBox -> (Vector -> Int) -> Bool
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


fromTwoVectors : Vector -> Vector -> BoundingBox
fromTwoVectors a b =
    BoundingBox a b
