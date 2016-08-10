module Koala exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Keyboard.Extra
import Time exposing (Time, second)
import List exposing (..)


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
    }


type alias Game =
    { character : Character
    , enemies : List Character
    , goal : Character
    , velocity : Int
    }


type alias Model =
    { keyboardModel : Keyboard.Extra.Model
    , game : Game
    , arrows : Position
    }


initialKoala : Character
initialKoala =
    Character (Position 0 0) "images/koala.png"


initialFlag : Character
initialFlag =
    Character (Position 1024 768) "images/flag.png"


initialEnemy : Character
initialEnemy =
    Character (Position 300 300) "images/enemy.png"


initialGame : Game
initialGame =
    Game initialKoala [ initialEnemy ] initialFlag 3


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyboardMsg keyMsg ->
            handleKeyboard model keyMsg

        Tick newTime ->
            ( { model | game = updateGame model.game model.arrows }, Cmd.none )


reinitKoala : Game -> Game
reinitKoala game =
    { game | character = initialKoala }


addEnemy : Game -> Game
addEnemy game =
    { game | enemies = initialEnemy :: game.enemies }


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
    { game | character = game.character |> moveCharacter (game.velocity ** arrows) }


stepEnemies : Game -> Game
stepEnemies game =
    { game | enemies = game.enemies |> moveCharacters game.velocity }


handleWinning : Game -> Game
handleWinning game =
    if isWinning game then
        game |> win
    else
        game


handleLoosing : Game -> Game
handleLoosing game =
    if isLoosing game then
        initialGame
    else
        game


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


moveCharacter : Position -> Character -> Character
moveCharacter vector character =
    { character | position = updatePosition character.position vector }


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
    enemies |> map (moveCharacter (velocity ** { x = 1, y = 1 }))



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
