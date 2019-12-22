module DifficultyTest exposing (suite)

import Difficulty
import Expect
import Fuzz exposing (intRange)
import Random exposing (maxInt, minInt)
import Test exposing (Test, describe, fuzz, test)


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
                "with a value less than 1, it returns the minimum"
              <|
                \n ->
                    Difficulty.fromInt n
                        |> Expect.equal Difficulty.min
            , fuzz (intRange 11 maxInt)
                "with a value more than 10, it returns the maximum"
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
