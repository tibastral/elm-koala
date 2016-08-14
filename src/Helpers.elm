module Helpers exposing (..)


ifonly : (a -> Bool) -> a -> a -> a
ifonly testFunction newVal a =
    if testFunction a then
        newVal
    else
        a


toPx : Int -> String
toPx i =
    (i |> toString) ++ "px"


spriteSize : number
spriteSize =
    128


halfSpriteSize : Int
halfSpriteSize =
    spriteSize / 2 |> round
