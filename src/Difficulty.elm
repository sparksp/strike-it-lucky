module Difficulty exposing
    ( Difficulty, max, min, fromInt
    , toInt, toString
    )

{-| Difficulty is a positive integer between 1 and 10.

@docs Difficulty, max, min, fromInt


# Conversion

@docs toInt, toString

-}


{-| -}
type Difficulty
    = Difficulty Int


{-| The largest Difficulty

    min == fromInt 10

-}
max : Difficulty
max =
    Difficulty maxInt


{-| The smallest Difficulty

    min == fromInt 1

-}
min : Difficulty
min =
    Difficulty minInt


{-| Create a `Difficulty` from an `Int`. If the Int is too small then `min` is returned. If the Int is too big then `max` is returned.

    fromInt 5
        |> toInt
        == 5

    fromInt -10 == min

    fromInt 20 == max

-}
fromInt : Int -> Difficulty
fromInt int =
    if int < minInt then
        Difficulty minInt

    else if int > maxInt then
        Difficulty maxInt

    else
        Difficulty int


{-| an Int of the Difficulty

    min |> toInt == 1

-}
toInt : Difficulty -> Int
toInt (Difficulty int) =
    int


{-| a String of the Difficulty

    max |> toString == "10"

-}
toString : Difficulty -> String
toString (Difficulty int) =
    String.fromInt int



--- PRIVATE


maxInt : Int
maxInt =
    10


minInt : Int
minInt =
    1
