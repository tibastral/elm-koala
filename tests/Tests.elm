module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Koala exposing (..)


all : Test
all =
    describe "A Test Suite"
        [ test "Addition" <|
            \() ->
                Expect.equal (3 + 7) 10
        , test "String.left" <|
            \() ->
                Expect.equal "a" (String.left 1 "abcdefg")
        , test "Collision" <|
            \() ->
                Expect.false "Collision didn't work" (collision initialKoala initialFlag)
        , test "CollisionPosition" <|
            \() ->
                Expect.false "Collision didn't work" (positionCollision initialKoala.position initialFlag.position)
        , test "CollisionPosition" <|
            \() ->
                Expect.false "Collision didn't work" (isLoosing initialGame)
        , test "CollisionPosition" <|
            \() ->
                Expect.true "Collision didn't work" (isLoosing (Game initialKoala [ initialKoala ] initialFlag 3))
        ]
