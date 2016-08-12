module Koala exposing (..)

import Html exposing (..)
import Html.App as Html
import Keyboard.Extra
import Time exposing (Time, second)
import Position exposing (Position)
import Game exposing (Game)


-- import Random


config :
    { fps : Float
    }
config =
    { fps = 60
    }


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


type Msg
    = KeyboardMsg Keyboard.Extra.Msg
    | Tick Time
    | GameMsg Game.Msg


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
            ( { model | game = Game.step model.game model.arrows }
            , if (round (Time.inMilliseconds newTime)) % 100 == 0 then
                Cmd.map GameMsg (Game.generateEnemiesRandom model.game)
              else
                Cmd.none
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



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Html.map GameMsg (Game.title model.game)
        , Html.map GameMsg (Game.charactersView model.game)
        ]
