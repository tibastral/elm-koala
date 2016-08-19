module BoundingBox exposing (..)

import Position exposing (Position)


type alias BoundingBox =
    { topLeft : Position
    , bottomRight : Position
    }


initialBoundingBox : BoundingBox
initialBoundingBox =
    fromTwoPositions (Position.fromXY 0 0) (Position.fromXY 1024 768)


add : Position -> Position -> BoundingBox -> Position
add position { x, y } boundingBox =
    { position
        | x = clamp boundingBox.topLeft.x boundingBox.bottomRight.x (position.x + x)
        , y = clamp boundingBox.topLeft.y boundingBox.bottomRight.y (position.y - y)
    }


touches : Position -> BoundingBox -> (Position -> Int) -> Bool
touches position boundingBox axis =
    axis position == axis boundingBox.topLeft || axis position == axis boundingBox.bottomRight


myWidth boundingBox =
    boundingBox.bottomRight.x - boundingBox.topLeft.x


myHeight boundingBox =
    boundingBox.bottomRight.y - boundingBox.topLeft.y


collision : BoundingBox -> BoundingBox -> Bool
collision a b =
    (abs (a.topLeft.x - b.topLeft.x) * 2 < (myWidth a) + (myWidth b))
        && (abs (a.topLeft.y - b.topLeft.y) * 2 < (myHeight a) + (myHeight b))



-- (a.topLeft.x |> between b.topLeft.x b.bottomRight.x) && (a.topLeft.y |> between b.topLeft.y b.topLeft.y)


between begin end number =
    number >= begin && number <= end


fromTwoPositions : Position -> Position -> BoundingBox
fromTwoPositions a b =
    BoundingBox a b


fromPositionAndDimensions a dimensions =
    fromTwoPositions a { a | x = a.x + dimensions.x, y = a.y + dimensions.y }
