module Character exposing (..)

-- import Html exposing (..)
-- import Html.Attributes exposing (..)
-- import Html.App as Html
-- import Helpers exposing (..)

import Position exposing (Position)
import List exposing (..)


type alias Character =
    { position : Position
    , path : String
    , speed : Position
    , id : Int
    }


initialKoala : Character
initialKoala =
    positionned 0 0 "images/koala.png" 0


initialFlag : Character
initialFlag =
    positionned 1024 768 "images/koala.png" 0


initialEnemy : Character
initialEnemy =
    positionned 300 300 "images/enemy.png" 1


positionned : Int -> Int -> String -> Int -> Character
positionned x y src id =
    Character (Position.fromXY x y) src (Position.initial) id


newEnemy : Int -> Character
newEnemy counter =
    { initialEnemy | id = counter }


move : Int -> Character -> Character
move velocity character =
    { character | position = Position.update character.position (velocity `Position.scalarMultiplication` character.speed) }


moveList : Int -> List Character -> List Character
moveList velocity enemies =
    enemies
        |> map (move velocity)


updateSpeed : Position -> Character -> Character
updateSpeed speed character =
    { character | speed = speed }


updateSpeeds : Int -> Position -> List Character -> List Character
updateSpeeds id speed characters =
    characters
        |> map
            (\e ->
                if e.id == id then
                    { e | speed = speed }
                else
                    e
            )


collision : Character -> Character -> Bool
collision a b =
    Position.collision a.position b.position
