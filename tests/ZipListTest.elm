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
            [ test "is a list of the given item" <|
                \_ ->
                    ZipList.singleton 1
                        |> expectEqualZipLists [] 1 []
            ]
        , describe "fromLists"
            [ test "is a list of the given items" <|
                \_ ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.toList
                        |> Expect.equal [ 1, 2, 3, 4, 5, 6 ]
            ]
        , describe "before"
            [ test "is a list of the items before selected" <|
                \_ ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.before
                        |> Expect.equal [ 1, 2 ]
            ]
        , describe "selected"
            [ test "is the selected item" <|
                \_ ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.selected
                        |> Expect.equal 3
            ]
        , describe "after"
            [ test "is a list of the items after selected" <|
                \_ ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.after
                        |> Expect.equal [ 4, 5, 6 ]
            ]
        , describe "toList"
            [ test "is a list of all items" <|
                \_ ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.toList
                        |> Expect.equal [ 1, 2, 3, 4, 5, 6 ]
            ]
        , describe "map"
            [ test "transforms each element of the list" <|
                \_ ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.map (\n -> n * 2)
                        |> expectEqualZipLists [ 2, 4 ] 6 [ 8, 10, 12 ]
            , test "can change items to a new item type" <|
                \_ ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.map String.fromInt
                        |> expectEqualZipLists [ "1", "2" ] "3" [ "4", "5", "6" ]
            ]
        , describe "mapSelected"
            [ test "transforms the selected element of the list" <|
                \_ ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.mapSelected (\n -> n * n)
                        |> expectEqualZipLists [ 1, 2 ] 9 [ 4, 5, 6 ]
            ]
        , describe "mapWithPosition"
            [ test "transforms each element of the list with a position" <|
                \_ ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> (ZipList.mapWithPosition <|
                                \position item ->
                                    ( position, item )
                           )
                        |> expectEqualZipLists [ ( ZipList.Before, 1 ), ( ZipList.Before, 2 ) ] ( ZipList.Selected, 3 ) [ ( ZipList.After, 4 ), ( ZipList.After, 5 ), ( ZipList.After, 6 ) ]
            ]
        , describe "update"
            [ test "replaces the current item in the list" <|
                \_ ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.update 9
                        |> expectEqualZipLists [ 1, 2 ] 9 [ 4, 5, 6 ]
            ]
        , describe "prepend"
            [ test "adds new items before the list" <|
                \_ ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.prepend [ 7, 8 ]
                        |> ZipList.before
                        |> Expect.equal [ 7, 8, 1, 2 ]
            ]
        , describe "append"
            [ test "adds new items after the list" <|
                \_ ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.append [ 7, 8 ]
                        |> ZipList.after
                        |> Expect.equal [ 4, 5, 6, 7, 8 ]
            ]
        , describe "select"
            [ test "does not change if no item passes the check" <|
                \_ ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.select (always False)
                        |> ZipList.selected
                        |> Expect.equal 3
            , test "selects the first item to pass the check" <|
                \_ ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.select isEven
                        |> expectEqualZipLists [ 1 ] 2 [ 3, 4, 5, 6 ]
            ]
        , describe "rewind"
            [ test "with a singleton, is the same list" <|
                \_ ->
                    ZipList.singleton 3
                        |> ZipList.rewind
                        |> Expect.equal (ZipList.singleton 3)
            , test "selects the first item" <|
                \_ ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.rewind
                        |> expectEqualZipLists [] 1 [ 2, 3, 4, 5, 6 ]
            ]
        , describe "next"
            [ test "with a singleton, is the same list" <|
                \_ ->
                    ZipList.singleton 3
                        |> ZipList.next
                        |> Expect.equal (ZipList.singleton 3)
            , test "selects the next item" <|
                \_ ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.next
                        |> expectEqualZipLists [ 1, 2, 3 ] 4 [ 5, 6 ]
            , test "at the end of a list, selects the last item" <|
                \_ ->
                    ZipList.fromLists [ 1, 2, 3, 4, 5 ] 6 []
                        |> ZipList.next
                        |> expectEqualZipLists [ 1, 2, 3, 4, 5 ] 6 []
            ]
        , describe "loop"
            [ test "with a singleton, is the same list" <|
                \_ ->
                    ZipList.singleton 3
                        |> ZipList.loop
                        |> Expect.equal (ZipList.singleton 3)
            , test "selects the next item" <|
                \_ ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.loop
                        |> expectEqualZipLists [ 1, 2, 3 ] 4 [ 5, 6 ]
            , test "at the end of a list, selects the first item" <|
                \_ ->
                    ZipList.fromLists [ 1, 2, 3, 4, 5 ] 6 []
                        |> ZipList.loop
                        |> expectEqualZipLists [] 1 [ 2, 3, 4, 5, 6 ]
            ]
        ]
