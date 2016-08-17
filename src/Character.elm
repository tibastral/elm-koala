module Character exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Helpers exposing (..)
import Position exposing (Position, BoundingBox, max)
import List exposing (..)


type alias Character =
    { position : Position
    , path : String
    , speed : Position
    , id : Int
    , looking : Looking
    }


type Looking
    = Right
    | Left


type Msg
    = No



{-
   Characters Initializers (Koala is the hero)
-}


initialKoala : Character
initialKoala =
    positionned 0 0 "assets/images/koala.png" 0


initialFlag : Character
initialFlag =
    positionned 1024 768 "assets/images/flag.png" 0


initialEnemy : Character
initialEnemy =
    positionned 300 300 "assets/images/enemy.png" 1


positionned : Int -> Int -> String -> Int -> Character
positionned x y src id =
    Character (Position.fromXY x y) src (Position.initial) id Right


newEnemy : Int -> Character
newEnemy counter =
    { initialEnemy | id = counter }


move : Int -> BoundingBox -> Character -> Character
move velocity boundingBox character =
    { character
        | position = Position.add character.position (velocity `Position.scalarMultiplication` character.speed) boundingBox
    }


invertSpeedIfEdge : Character -> Character
invertSpeedIfEdge character =
    let
        position =
            character.position

        speed =
            character.speed

        newSpeed =
            if position.x == 0 || position.x == (.x Position.max) then
                { speed | x = 0 - speed.x }
            else if position.y <= 0 || position.y >= (.y Position.max) then
                { speed | y = 0 - speed.y }
            else
                speed
    in
        { character | speed = newSpeed }


moveList : Int -> BoundingBox -> List Character -> List Character
moveList velocity boundingBox enemies =
    enemies
        |> map (move velocity boundingBox)
        |> map invertSpeedIfEdge


updateLooking looking speed =
    if speed.x > 0 then
        Right
    else if speed.x < 0 then
        Left
    else
        looking


updateSpeed : Position -> Character -> Character
updateSpeed speed character =
    { character
        | speed = speed
        , looking = updateLooking character.looking speed
    }


updateSpeeds : Int -> Position -> List Character -> List Character
updateSpeeds id speed characters =
    characters
        |> map
            (\e ->
                if e.id == id then
                    e |> updateSpeed speed
                else
                    e
            )


collision : Character -> Character -> Bool
collision a b =
    Position.collision a.position b.position


baseStyle { x, y } =
    [ ( "position", "absolute" )
    , ( "left", x |> toPx )
    , ( "top", y |> toPx )
    ]


myStyle { position, speed, looking } =
    if looking == Right then
        baseStyle position
    else
        ( "transform", "scaleX(-1)" ) :: baseStyle position


view : Character -> Html Msg
view character =
    div
        [ style
            (myStyle character)
        ]
        [ img [ src character.path, width spriteSize, height spriteSize ] [] ]


viewList : List Character -> Html Msg
viewList characters =
    div []
        (characters
            |> map view
        )
