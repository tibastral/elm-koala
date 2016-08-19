module Character exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Helpers exposing (..)
import Position exposing (Position)
import BoundingBox exposing (BoundingBox)
import List exposing (..)


type alias Character =
    { position : Position
    , path : String
    , speed : Position
    , id : Int
    , looking : Looking
    , spriteSize : Position
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
    positionned 0 0 "assets/images/koala.png" 0 24 48


initialFlag : Character
initialFlag =
    positionned (1024 - 117) (768 - 104) "assets/images/flag.png" 0 117 104


initialEnemy : Character
initialEnemy =
    positionned 300 300 "assets/images/enemy.png" 1 82 84



-- positionned : Int -> Int -> String -> Int -> Character


positionned x y src id width height =
    Character (Position.fromXY x y) src (Position.initial) id Right (Position.fromXY width height)


newEnemy : Int -> Character
newEnemy counter =
    { initialEnemy | id = counter }


add : Character -> Position -> BoundingBox -> Position
add character { x, y } boundingBox =
    let
        position =
            character.position
    in
        { position
            | x = clamp boundingBox.topLeft.x (boundingBox.bottomRight.x - character.spriteSize.x) (position.x + x)
            , y = clamp boundingBox.topLeft.y (boundingBox.bottomRight.y - character.spriteSize.y) (position.y - y)
        }


move : Int -> BoundingBox -> Character -> Character
move velocity outsideBox character =
    { character
        | position =
            add
                character
                (velocity `Position.scalarMultiplication` character.speed)
                outsideBox
    }


invertSpeedIfEdge : BoundingBox -> Character -> Character
invertSpeedIfEdge boundingBox character =
    let
        position =
            character.position

        speed =
            character.speed

        newSpeed =
            if BoundingBox.touches position boundingBox .x then
                { speed | x = 0 - speed.x }
            else if BoundingBox.touches position boundingBox .y then
                { speed | y = 0 - speed.y }
            else
                speed
    in
        { character | speed = newSpeed }


moveList : Int -> BoundingBox -> List Character -> List Character
moveList velocity boundingBox enemies =
    enemies
        |> map (move velocity boundingBox)
        |> map (invertSpeedIfEdge boundingBox)


updateLooking : Looking -> Position -> Looking
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
    BoundingBox.collision
        (BoundingBox.fromPositionAndDimensions a.position a.spriteSize)
        (BoundingBox.fromPositionAndDimensions b.position b.spriteSize)


baseStyle : Position -> List ( String, String )
baseStyle { x, y } =
    [ ( "position", "absolute" )
    , ( "left", x |> toPx )
    , ( "top", y |> toPx )
    , ( "border", "1px solid black" )
    ]


myStyle : Character -> List ( String, String )
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
        [ img [ style [ ( "display", "block" ) ], src character.path, width character.spriteSize.x, height character.spriteSize.y ] [] ]


viewList : List Character -> Html Msg
viewList characters =
    div []
        (characters
            |> map view
        )
