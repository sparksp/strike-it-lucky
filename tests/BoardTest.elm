module BoardTest exposing (suite)

import Board
import Expect
import Test exposing (Test, describe, test)


newBoardTiles : List Board.Tile
newBoardTiles =
    Board.Selection Board.NotSelected
        :: List.repeat 8 Board.Future
        ++ [ Board.Final Board.FinalQuestion ]


suite : Test
suite =
    describe "Board"
        [ describe "new"
            [ test "is not selected" <|
                \_ ->
                    Board.new
                        |> Board.selected
                        |> Expect.equal (Board.Selection Board.NotSelected)
            , test "has no answers" <|
                \_ ->
                    Board.new
                        |> Board.map identity
                        |> Expect.equal newBoardTiles
            ]
        , describe "answer"
            [ test "with nothing selected, nothing is selected" <|
                \_ ->
                    Board.new
                        |> Board.answer Board.Pass
                        |> Board.map identity
                        |> Expect.equal newBoardTiles
            , test "with a Question selected, is not selected" <|
                \_ ->
                    Board.new
                        |> Board.loading Board.Middle
                        |> Board.question Board.Single
                        |> Board.answer Board.Pass
                        |> Board.selected
                        |> Expect.equal (Board.Selection Board.NotSelected)
            ]
        , describe "map"
            [ test "maps over everything" <|
                \_ ->
                    let
                        mapper : Board.Tile -> String
                        mapper tile =
                            case tile of
                                Board.Answer (Board.AnswerAt _ Board.Pass) ->
                                    "Pass"

                                Board.Answer (Board.AnswerAt _ Board.Fail) ->
                                    "Fail"

                                Board.Selection _ ->
                                    "Selection"

                                Board.Future ->
                                    "Future"

                                Board.Final _ ->
                                    "Final"
                    in
                    Board.new
                        |> Board.loading Board.Middle
                        |> Board.question Board.Single
                        |> Board.answer Board.Pass
                        |> Board.loading Board.Top
                        |> Board.question Board.Team
                        |> Board.answer Board.Fail
                        |> Board.map mapper
                        |> Expect.equal [ "Pass", "Fail", "Selection", "Future", "Future", "Future", "Future", "Future", "Future", "Final" ]
            ]
        ]
