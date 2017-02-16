module Koala exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Keyboard.Extra
import List exposing (..)
import Time exposing (Time, second)
import Vector exposing (Vector)
import Game exposing (Game)


-- import Random


config :
    { fps : Float
    }
config =
    { fps = 60
    }


main : Program Never Model Msg
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
    , arrows : Vector
    }


type Msg
    = KeyboardMsg Keyboard.Extra.Msg
    | Tick Time
    | GameMsg Game.Msg


initialKeyboard : Keyboard.Extra.Model
initialKeyboard =
    Tuple.first Keyboard.Extra.init


initialModel : Model
initialModel =
    Model initialKeyboard Game.initial Vector.initial


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


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


generateRandomEnemies time model =
    if floor (Time.inMilliseconds time) % 100 == 0 then
        model.game
            |> Game.generateRandomEnemies
            |> Cmd.map GameMsg
    else
        Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyboardMsg keyMsg ->
            handleKeyboard model keyMsg

        Tick newTime ->
            ( { model | game = Game.step model.game model.arrows }
            , model |> generateRandomEnemies newTime
            )

        GameMsg msg ->
            ( { model | game = Game.update msg model.game }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardMsg Keyboard.Extra.subscriptions
        , Time.every (second / config.fps) Tick
        ]


title : Model -> Html Msg
title model =
    h1 [ style [ ( "position", "absolute" ) ] ]
        [ ("score : "
            ++ (((model.game.enemies |> length) - 1)
                    |> toString
               )
          )
            |> text
        , " hi-score : " |> text
        , model.game.hiScore |> toString |> text
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ model |> title
        , Game.view model.game
            |> Html.map GameMsg
        ]
