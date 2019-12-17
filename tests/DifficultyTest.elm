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
              <|
                \n ->
                    Difficulty.fromInt n
                        |> Difficulty.toInt
                        |> Expect.equal n
            , test "Difficulty cannot be zero" <|
                \_ ->
                    Difficulty.fromInt 0
                        |> Expect.equal Difficulty.min
            , fuzz (intRange minInt -1)
                "Difficulty cannot be negative"
              <|
                \n ->
                    Difficulty.fromInt n
                        |> Expect.equal Difficulty.min
            , fuzz (intRange 11 maxInt)
                "Difficulty cannot be greater than 10"
              <|
                \n ->
                    Difficulty.fromInt n
                        |> Expect.equal Difficulty.max
            ]
        , describe "toString"
            [ fuzz (intRange 1 10)
                "Returns a String of the given difficulty"
              <|
                \n ->
                    Difficulty.fromInt n
                        |> Difficulty.toString
                        |> Expect.equal (String.fromInt n)
            ]
        , describe "min"
            [ test "is 1" <|
                \_ ->
                    Difficulty.min
                        |> Difficulty.toInt
                        |> Expect.equal 1
            ]
        , describe "max"
            [ test "is 10" <|
                \_ ->
                    Difficulty.max
                        |> Difficulty.toInt
                        |> Expect.equal 10
            ]
        ]
