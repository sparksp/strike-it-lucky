module DifficultyTest exposing (suite)

import Difficulty exposing (Difficulty)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, intRange)
import Random exposing (maxInt, minInt)
import Test exposing (..)


suite : Test
suite =
    describe "Difficulty"
        [ describe "fromInt"
            [ fuzz (intRange 1 10)
                "Difficulty can be 1..10"
                (\n ->
                    Maybe.map Difficulty.toInt (Difficulty.fromInt n)
                        |> Expect.equal (Just n)
                )
            , test "Difficulty cannot be zero"
                (\() ->
                    Difficulty.fromInt 0
                        |> Expect.equal Nothing
                )
            , fuzz (intRange minInt -1)
                "Difficulty cannot be negative"
                (\n ->
                    Difficulty.fromInt n
                        |> Expect.equal Nothing
                )
            , fuzz (intRange 11 maxInt)
                "Difficulty cannot be greater than 10"
                (\n ->
                    Difficulty.fromInt n
                        |> Expect.equal Nothing
                )
            ]
        , describe "toString"
            [ fuzz (intRange 1 10)
                "Returns a String of the given difficulty"
                (\n ->
                    Maybe.map Difficulty.toString (Difficulty.fromInt n)
                        |> Expect.equal (Just (String.fromInt n))
                )
            ]
        , describe "min"
            [ test "is 1"
                (\() ->
                    Difficulty.toInt Difficulty.min
                        |> Expect.equal 1
                )
            ]
        , describe "max"
            [ test "is 10"
                (\() ->
                    Difficulty.toInt Difficulty.max
                        |> Expect.equal 10
                )
            ]
        ]
