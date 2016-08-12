module Koala exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Keyboard.Extra
import Time exposing (Time, second)
import List exposing (..)
import Random
import Helpers exposing (..)
import Position exposing (Position)
import Character exposing (Character)
import Game exposing (Game)


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


type alias Model =
    { keyboardModel : Keyboard.Extra.Model
    , game : Game
    , arrows : Position
    }


initialKeyboard : Keyboard.Extra.Model
initialKeyboard =
    fst Keyboard.Extra.init


initialModel : Model
initialModel =
    Model initialKeyboard Game.initial Position.initial


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
            ( { model | game = Game.update model.game model.arrows }
            , if (round (Time.inMilliseconds newTime)) % 100 == 0 then
                generateEnemiesRandom model.game.enemies
              else
                Cmd.none
            )

        UpdateSpeed enemyId speed ->
            ( { model | game = Game.updateEnemySpeed model.game enemyId speed }, Cmd.none )


config :
    { fps : Float
    }
config =
    { fps = 60
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardMsg Keyboard.Extra.subscriptions
        , Time.every (second / config.fps) Tick
        ]



-- VIEW


characterView : Character -> Html Msg
characterView { position, path } =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "left", position.x |> toPx )
            , ( "top", position.y |> toPx )
            ]
        ]
        [ img [ src path, width Position.spriteSize, height Position.spriteSize ] [] ]


charactersView : Game -> Html Msg
charactersView game =
    div []
        (game
            |> Game.characters
            |> map characterView
        )


title : Model -> Html Msg
title { game } =
    h1 [ style [ ( "position", "absolute" ) ] ]
        [ game.enemies
            |> length
            |> toString
            |> text
        ]


view : Model -> Html Msg
view model =
    div []
        [ model |> title
        , charactersView model.game
        ]
