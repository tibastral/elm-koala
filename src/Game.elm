module Game exposing (..)

import Position exposing (Position)
import Character exposing (Character)
import Helpers exposing (..)
import List exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Random


type Msg
    = UpdateSpeed Int Position


type alias Game =
    { character : Character
    , enemies : List Character
    , goal : Character
    , velocity : Int
    , enemiesCounter : Int
    }


initial : Game
initial =
    Game Character.initialKoala [ Character.initialEnemy ] Character.initialFlag 3 2


updateEnemySpeed : Game -> Int -> Position -> Game
updateEnemySpeed game enemyId speed =
    { game | enemies = Character.updateSpeeds enemyId speed game.enemies }


reinitKoala : Game -> Game
reinitKoala game =
    { game | character = Character.initialKoala }


addEnemy : Game -> Game
addEnemy game =
    { game
        | enemies = (Character.newEnemy game.enemiesCounter) :: game.enemies
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


stepCharacter : Position -> Game -> Game
stepCharacter arrows game =
    { game
        | character =
            game.character
                |> Character.updateSpeed arrows
                |> Character.move game.velocity
    }


stepEnemies : Game -> Game
stepEnemies game =
    { game
        | enemies =
            game.enemies
                |> Character.moveList game.velocity
    }


handleWinning : Game -> Game
handleWinning game =
    game
        |> ifonly isWinning (game |> win)


handleLoosing : Game -> Game
handleLoosing game =
    game
        |> ifonly isLoosing initial


step : Game -> Position -> Game
step game arrows =
    game
        |> handleWinning
        |> handleLoosing
        |> stepCharacter arrows
        |> stepEnemies


isWinning : Game -> Bool
isWinning { character, goal } =
    character |> Character.collision goal


isLoosing : Game -> Bool
isLoosing { enemies, character } =
    enemies |> any (Character.collision character)


characters : Game -> List Character
characters { character, goal, enemies } =
    character
        :: goal
        :: enemies


update : Msg -> Game -> Game
update msg game =
    case msg of
        UpdateSpeed enemyId speed ->
            updateEnemySpeed game enemyId speed


title : Game -> Html Msg
title { enemies } =
    h1 [ style [ ( "position", "absolute" ) ] ]
        [ enemies
            |> length
            |> toString
            |> text
        ]


updateSpeedGenerator : Int -> Random.Generator Msg
updateSpeedGenerator id =
    Random.map2
        (\x y -> UpdateSpeed id { x = x, y = y })
        (Random.int -1 1)
        (Random.int -1 1)


generateEnemiesRandom : Game -> Cmd Msg
generateEnemiesRandom { enemies } =
    enemies
        |> map (\e -> Random.generate identity (updateSpeedGenerator e.id))
        |> Cmd.batch


characterView : Character -> Html Msg
characterView { position, path } =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "left", position.x |> toPx )
            , ( "top", position.y |> toPx )
            ]
        ]
        [ img [ src path, width spriteSize, height spriteSize ] [] ]


charactersView : Game -> Html Msg
charactersView game =
    div []
        (game
            |> characters
            |> map characterView
        )


view : Game -> Html Msg
view game =
    div []
        [ title game
        , (charactersView game)
        ]
