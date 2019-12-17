module ZipListTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, intRange)
import Random exposing (maxInt, minInt)
import Test exposing (..)
import ZipList exposing (ZipList)


isEven : Int -> Bool
isEven n =
    (n // 2) * 2 == n


expectEqualZipLists : List a -> a -> List a -> ZipList a -> Expectation
expectEqualZipLists before current after zipList =
    Expect.equal (ZipList.fromLists before current after) zipList


suite : Test
suite =
    describe "ZipList"
        [ describe "singleton"
            [ test "is a list of the given item"
                (\() ->
                    ZipList.singleton 1
                        |> ZipList.toList
                        |> Expect.equal [ 1 ]
                )
            ]
        , describe "fromLists"
            [ test "is a list of the given items"
                (\() ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.toList
                        |> Expect.equal [ 1, 2, 3, 4, 5, 6 ]
                )
            ]
        , describe "before"
            [ test "is a list of the items before current"
                (\() ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.before
                        |> Expect.equal [ 1, 2 ]
                )
            ]
        , describe "selected"
            [ test "is the current item"
                (\() ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.selected
                        |> Expect.equal 3
                )
            ]
        , describe "after"
            [ test "is a list of the items after current"
                (\() ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.after
                        |> Expect.equal [ 4, 5, 6 ]
                )
            ]
        , describe "toList"
            [ test "is a list of all items"
                (\() ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.toList
                        |> Expect.equal [ 1, 2, 3, 4, 5, 6 ]
                )
            ]
        , describe "map"
            [ test "transforms each element of the list"
                (\() ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.map (\n -> n * 2)
                        |> ZipList.toList
                        |> Expect.equal [ 2, 4, 6, 8, 10, 12 ]
                )
            , test "can change items to a new item type"
                (\() ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.map String.fromInt
                        |> ZipList.toList
                        |> Expect.equal [ "1", "2", "3", "4", "5", "6" ]
                )
            ]
        , describe "update"
            [ test "replaces the current item in the list" <|
                \_ ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.update 9
                        |> expectEqualZipLists [ 1, 2 ] 9 [ 4, 5, 6 ]
            ]
        , describe "prepend"
            [ test "adds new items before the list"
                (\() ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.prepend [ 7, 8 ]
                        |> ZipList.before
                        |> Expect.equal [ 7, 8, 1, 2 ]
                )
            ]
        , describe "append"
            [ test "adds new items after the list"
                (\() ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.append [ 7, 8 ]
                        |> ZipList.after
                        |> Expect.equal [ 4, 5, 6, 7, 8 ]
                )
            ]
        , describe "select"
            [ test "does not change if no item passes the check"
                (\() ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.select (always False)
                        |> ZipList.selected
                        |> Expect.equal 3
                )
            , test "selects the first item to pass the check"
                (\() ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.select isEven
                        |> Expect.equal (ZipList.fromLists [ 1 ] 2 [ 3, 4, 5, 6 ])
                )
            ]
        , describe "rewind"
            [ test "with a singleton, is the same list"
                (\() ->
                    ZipList.singleton 3
                        |> ZipList.rewind
                        |> Expect.equal (ZipList.singleton 3)
                )
            , test "selects the first item"
                (\() ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.rewind
                        |> Expect.equal (ZipList.fromLists [] 1 [ 2, 3, 4, 5, 6 ])
                )
            ]
        , describe "next"
            [ test "with a singleton, is the same list"
                (\() ->
                    ZipList.singleton 3
                        |> ZipList.next
                        |> Expect.equal (ZipList.singleton 3)
                )
            , test "selects the next item"
                (\() ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.next
                        |> Expect.equal (ZipList.fromLists [ 1, 2, 3 ] 4 [ 5, 6 ])
                )
            , test "at the end of a list, selects the first item"
                (\() ->
                    ZipList.fromLists [ 1, 2, 3, 4, 5 ] 6 []
                        |> ZipList.next
                        |> Expect.equal (ZipList.fromLists [] 1 [ 2, 3, 4, 5, 6 ])
                )
            ]
        ]
