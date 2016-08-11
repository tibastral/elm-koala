module Koala exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Keyboard.Extra
import Time exposing (Time, second)
import List exposing (..)
import Random


-- import Random


main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Position =
    { x : Int
    , y : Int
    }


type alias Character =
    { position : Position
    , path : String
    , speed : Position
    , id : Int
    }


type alias Game =
    { character : Character
    , enemies : List Character
    , goal : Character
    , velocity : Int
    , enemiesCounter : Int
    }


type alias Model =
    { keyboardModel : Keyboard.Extra.Model
    , game : Game
    , arrows : Position
    }


initialKoala : Character
initialKoala =
    Character (Position 0 0) "images/koala.png" (Position 0 0) -1


initialFlag : Character
initialFlag =
    Character (Position 1024 768) "images/flag.png" (Position 0 0) -2


initialEnemy : Character
initialEnemy =
    Character (Position 300 300) "images/enemy.png" (Position -1 -1) 1


initialGame : Game
initialGame =
    Game initialKoala [ initialEnemy ] initialFlag 3 2


initialPosition : Position
initialPosition =
    Position 0 0


initialKeyboard : Keyboard.Extra.Model
initialKeyboard =
    fst Keyboard.Extra.init


initialModel : Model
initialModel =
    Model initialKeyboard initialGame initialPosition


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = KeyboardMsg Keyboard.Extra.Msg
    | Tick Time
    | UpdateSpeed Int Position


handleKeyboard : Model -> Keyboard.Extra.Msg -> ( Model, Cmd Msg )
handleKeyboard model keyMsg =
    let
        ( keyboardModel, keyboardCmd ) =
            Keyboard.Extra.update keyMsg model.keyboardModel
    in
        ( { model
            | keyboardModel = keyboardModel
            , arrows = Keyboard.Extra.arrows keyboardModel
          }
        , Cmd.map KeyboardMsg keyboardCmd
        )


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


updateEnemySpeed : Game -> Int -> Position -> Game
updateEnemySpeed game enemyId speed =
    { game | enemies = updateSpeeds enemyId speed game.enemies }


updateSpeedGenerator : Int -> Random.Generator Msg
updateSpeedGenerator id =
    Random.map2
        (\x y -> UpdateSpeed id { x = x, y = y })
        (Random.int -1 1)
        (Random.int -1 1)


generateEnemiesRandom : List Character -> Cmd Msg
generateEnemiesRandom enemies =
    enemies
        |> map (\e -> Random.generate identity (updateSpeedGenerator e.id))
        |> Cmd.batch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyboardMsg keyMsg ->
            handleKeyboard model keyMsg

        Tick newTime ->
            ( { model | game = updateGame model.game model.arrows }
            , if (round (Time.inMilliseconds newTime)) % 100 == 0 then
                generateEnemiesRandom model.game.enemies
              else
                Cmd.none
            )

        UpdateSpeed enemyId speed ->
            ( { model | game = updateEnemySpeed model.game enemyId speed }, Cmd.none )


reinitKoala : Game -> Game
reinitKoala game =
    { game | character = initialKoala }


newEnemy : Int -> Character
newEnemy counter =
    { initialEnemy | id = counter }


addEnemy : Game -> Game
addEnemy game =
    { game
        | enemies = (newEnemy game.enemiesCounter) :: game.enemies
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


updateSpeed : Position -> Character -> Character
updateSpeed speed character =
    { character | speed = speed }


stepCharacter : Position -> Game -> Game
stepCharacter arrows game =
    { game
        | character =
            game.character
                |> updateSpeed arrows
                |> moveCharacter game.velocity
    }


stepEnemies : Game -> Game
stepEnemies game =
    { game
        | enemies =
            game.enemies
                |> moveCharacters game.velocity
    }


ifonly : (a -> Bool) -> a -> a -> a
ifonly testFunction newVal a =
    if testFunction a then
        newVal
    else
        a


handleWinning : Game -> Game
handleWinning game =
    game
        |> ifonly isWinning (game |> win)


handleLoosing : Game -> Game
handleLoosing game =
    game
        |> ifonly isLoosing initialGame


updateGame : Game -> Position -> Game
updateGame game arrows =
    game
        |> handleWinning
        |> handleLoosing
        |> stepCharacter arrows
        |> stepEnemies


isWinning : Game -> Bool
isWinning { character, goal } =
    character |> collision goal


isLoosing : Game -> Bool
isLoosing { enemies, character } =
    enemies |> any (collision character)


spriteSize : number
spriteSize =
    128


halfSpriteSize : Int
halfSpriteSize =
    spriteSize / 2 |> round


collision : Character -> Character -> Bool
collision a b =
    positionCollision a.position b.position


positionCollision : Position -> Position -> Bool
positionCollision a b =
    all (lateralCollision a b) [ .x, .y ]


lateralCollision : Position -> Position -> (Position -> Int) -> Bool
lateralCollision a b axis =
    abs (axis a - axis b) < halfSpriteSize


moveCharacter : Int -> Character -> Character
moveCharacter velocity character =
    { character | position = updatePosition character.position (velocity ** character.speed) }


normalize : number -> number -> number
normalize =
    clamp 0


updatePosition : Position -> Position -> Position
updatePosition position { x, y } =
    { position
        | x = normalize 1000 (position.x + x)
        , y = normalize 768 (position.y - y)
    }


(**) : number -> Position -> Position
(**) velocity { x, y } =
    { x = x * velocity, y = y * velocity }


moveCharacters : Int -> List Character -> List Character
moveCharacters velocity enemies =
    enemies
        |> map (moveCharacter velocity)



-- SUBSCRIPTIONS


config :
    { fps : Float
    }
config =
    { fps = 60
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardMsg Keyboard.Extra.subscriptions
        , Time.every (second / config.fps) Tick
        ]



-- VIEW


toPx : Int -> String
toPx i =
    (i |> toString) ++ "px"


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


characters : Game -> List Character
characters { character, goal, enemies } =
    character
        :: goal
        :: enemies


title : Model -> Html Msg
title { game } =
    h1 [ style [ ( "position", "absolute" ) ] ]
        [ game.enemies
            |> length
            |> toString
            |> text
        ]


charactersView : Game -> Html Msg
charactersView game =
    div []
        (game
            |> characters
            |> map characterView
        )


view : Model -> Html Msg
view model =
    div []
        [ model |> title
        , charactersView model.game
        ]
