module Difficulty exposing (Difficulty, max, min, fromInt, toInt, toString)

{-| Difficulty is a positive integer between 1 and 10.

@docs Difficulty, max, min, fromInt, toInt, toString

-}


{-| -}
type Difficulty
    = Difficulty Int


{-| The largest Difficulty. Useful as a default value or with `Maybe.withDefault`.

    fromInt 9 |> Maybe.withDefault max

-}
max : Difficulty
max =
    Difficulty maxInt


{-| The smallest Difficulty. Useful as a default value or with `Maybe.withDefault`.

    fromInt 3 |> Maybe.withDefault min

-}
min : Difficulty
min =
    Difficulty minInt


{-| Create a `Difficulty` from an `Int`.
-}
fromInt : Int -> Maybe Difficulty
fromInt int =
    if isDifficulty int then
        Just (Difficulty int)

    else
        Nothing


{-| -}
toInt : Difficulty -> Int
toInt (Difficulty int) =
    int


{-| -}
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


isDifficulty : Int -> Bool
isDifficulty int =
    if int < minInt then
        False

    else if int > maxInt then
        False

    else
        True
