module Game exposing (..)

import Vector exposing (Vector)
import BoundingBox exposing (BoundingBox)
import Character exposing (Character)
import Helpers exposing (..)
import List exposing (..)
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Helpers exposing (..)
import Random


type Msg
    = UpdateSpeed Int Vector
    | EnemiesMsg Character.Msg
    | HeroMsg Character.Msg
    | GoalMsg Character.Msg


type alias Game =
    { character : Character
    , enemies : List Character
    , goal : Character
    , velocity : Int
    , enemiesCounter : Int
    , hiScore : Int
    , boundingBox : BoundingBox
    }


initial : Game
initial =
    Game Character.initialKoala [ Character.initialEnemy ] Character.initialFlag 3 2 0 BoundingBox.initialBoundingBox


updateEnemySpeed : Game -> Int -> Vector -> Game
updateEnemySpeed game enemyId speed =
    { game | enemies = Character.updateSpeeds enemyId speed game.enemies }


reinitKoala : Game -> Game
reinitKoala game =
    { game | character = Character.initialKoala }


addEnemy : Game -> Game
addEnemy game =
    let
        newEnemy =
            Character.newEnemy game.enemiesCounter
    in
        { game
            | enemies = { newEnemy | spriteSize = Vector.fromXY (newEnemy.spriteSize.x + game.enemiesCounter * 5) (newEnemy.spriteSize.y + game.enemiesCounter * 5) } :: game.enemies
            , enemiesCounter = game.enemiesCounter + 1
        }


increaseVelocity : Game -> Game
increaseVelocity game =
    { game | velocity = game.velocity + 1 }


win : Game -> Game
win game =
    game
        |> reinitKoala
        |> addEnemy
        |> increaseVelocity


stepCharacter : Vector -> Game -> Game
stepCharacter arrows game =
    { game
        | character =
            game.character
                |> Character.updateSpeed arrows
                |> Character.move game.velocity game.boundingBox
    }


stepEnemies : Game -> Game
stepEnemies game =
    { game
        | enemies =
            game.enemies
                |> Character.moveList game.velocity game.boundingBox
    }


step : Game -> Vector -> Game
step game arrows =
    game
        |> handleWinning
        |> handleLosing
        |> stepCharacter arrows
        |> stepEnemies


isWinning : Game -> Bool
isWinning { character, goal } =
    character |> Character.collision goal


handleWinning : Game -> Game
handleWinning game =
    game
        |> ifonly isWinning (game |> win)


isLosing : Game -> Bool
isLosing { character, enemies } =
    enemies |> any (Character.collision character)


handleLosing : Game -> Game
handleLosing game =
    let
        hiScore : Int
        hiScore =
            case List.maximum [ game.enemiesCounter - 2, game.hiScore ] of
                Just i ->
                    i

                Nothing ->
                    0
    in
        game
            |> ifonly isLosing { initial | hiScore = hiScore }


update : Msg -> Game -> Game
update msg game =
    case msg of
        UpdateSpeed enemyId speed ->
            updateEnemySpeed game enemyId speed

        -- For now, this code isn't handling messages passed to the Hero, Goal or Enemies
        _ ->
            game


updateSpeedGenerator : Int -> Random.Generator Msg
updateSpeedGenerator id =
    Random.map2
        (\x y -> UpdateSpeed id { x = x, y = y })
        (Random.int -1 1)
        (Random.int -1 1)


generateRandomEnemies : Game -> Cmd Msg
generateRandomEnemies { enemies } =
    enemies
        |> map (\e -> Random.generate identity (updateSpeedGenerator e.id))
        |> Cmd.batch


view : Game -> Html Msg
view game =
    div
        [ style
            [ ( "width", game.boundingBox.bottomRight.x |> toPx )
            , ( "height", game.boundingBox.bottomRight.y |> toPx )
            , ( "border", "1px solid black" )
            ]
        ]
        [ game.enemies |> Character.viewList |> Html.map EnemiesMsg
        , game.goal |> Character.view |> Html.map GoalMsg
        , game.character |> Character.view |> Html.map HeroMsg
        ]
