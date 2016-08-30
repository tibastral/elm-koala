module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import BoundingBox exposing (..)
import Character exposing (..)
import Koala exposing (..)
import Game


all : Test
all =
    describe "A Test Suite"
        [ test "Collision" <|
            \() ->
                Expect.false "Collision didn't work" (Character.collision initialKoala initialFlag)
        , test "CollisionVector" <|
            \() ->
                Expect.false "Initial state of game is losing" (Game.isLosing Game.initial)
        , test "CollisionVector" <|
            \() ->
                let
                    losingCharacter =
                        initialEnemy

                    initialGame =
                        Game.initial

                    losingGame =
                        { initialGame | character = losingCharacter }
                in
                    Expect.true "Losing game didn't lose" (Game.isLosing losingGame)
        ]
