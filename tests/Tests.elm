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
        , test "CollisionPosition" <|
            \() ->
                Expect.false "Initial state of game is loosing" (Game.isLoosing Game.initial)
        , test "CollisionPosition" <|
            \() ->
                let
                    loosingCharacter =
                        initialEnemy

                    initialGame =
                        Game.initial

                    loosingGame =
                        { initialGame | character = loosingCharacter }
                in
                    Expect.true "Loosing game didn't loose" (Game.isLoosing loosingGame)
        ]
