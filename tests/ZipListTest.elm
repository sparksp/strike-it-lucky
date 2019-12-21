module ZipListTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (int, list)
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, test)
import Tuple
import ZipList exposing (ZipList)


isEven : Int -> Bool
isEven n =
    (n // 2) * 2 == n


double : number -> number
double =
    (*) 2


square : number -> number
square =
    (^) 2


expectEqualZipLists : List a -> a -> List a -> ZipList a -> Expectation
expectEqualZipLists before current after zipList =
    Expect.equal (ZipList.fromLists before current after) zipList


suite : Test
suite =
    describe "ZipList"
        [ describe "singleton"
            [ fuzz int "is a list of the given item" <|
                \selected ->
                    ZipList.singleton selected
                        |> expectEqualZipLists [] selected []
            ]
        , describe "fromLists"
            [ fuzz3 (list int) int (list int) "is a list of the given items" <|
                \before selected after ->
                    ZipList.fromLists before selected after
                        |> expectEqualZipLists before selected after
            ]
        , describe "before"
            [ fuzz (list int) "is a list of the items before selected" <|
                \before ->
                    ZipList.fromLists before 1 [ 2, 3, 4 ]
                        |> ZipList.before
                        |> Expect.equal before
            ]
        , describe "selected"
            [ fuzz int "is the selected item" <|
                \selected ->
                    ZipList.fromLists [ 1, 2, 3 ] selected [ 4, 5, 6 ]
                        |> ZipList.selected
                        |> Expect.equal selected
            ]
        , describe "after"
            [ fuzz (list int) "is a list of the items after selected" <|
                \after ->
                    ZipList.fromLists [ 1, 2, 3 ] 4 after
                        |> ZipList.after
                        |> Expect.equal after
            ]
        , describe "toList"
            [ fuzz3 (list int) int (list int) "is a list of all items" <|
                \before selected after ->
                    ZipList.fromLists before selected after
                        |> ZipList.toList
                        |> Expect.equal (before ++ selected :: after)
            ]
        , describe "length"
            [ fuzz3 (list int) int (list int) "is the count of all items" <|
                \before selected after ->
                    ZipList.fromLists before selected after
                        |> ZipList.length
                        |> Expect.equal (List.length <| before ++ selected :: after)
            , fuzz int "with a singleton, is 1" <|
                \selected ->
                    ZipList.singleton selected
                        |> ZipList.length
                        |> Expect.equal 1
            ]
        , describe "map"
            [ fuzz3 (list int) int (list int) "transforms each element of the list" <|
                \before selected after ->
                    ZipList.fromLists before selected after
                        |> ZipList.map double
                        |> expectEqualZipLists (List.map double before) (double selected) (List.map double after)
            , fuzz3 (list int) int (list int) "can change items to a new item type" <|
                \before selected after ->
                    ZipList.fromLists before selected after
                        |> ZipList.map String.fromInt
                        |> expectEqualZipLists (List.map String.fromInt before) (String.fromInt selected) (List.map String.fromInt after)
            ]
        , describe "mapSelected"
            [ fuzz int "transforms the selected element of the list" <|
                \selected ->
                    ZipList.fromLists [ 1, 2, 3 ] selected [ 4, 5 ]
                        |> ZipList.mapSelected square
                        |> expectEqualZipLists [ 1, 2, 3 ] (square selected) [ 4, 5 ]
            ]
        , describe "mapWithPosition"
            [ test "transforms each element of the list with a position" <|
                \_ ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.mapWithPosition Tuple.pair
                        |> expectEqualZipLists [ ( ZipList.Before, 1 ), ( ZipList.Before, 2 ) ] ( ZipList.Selected, 3 ) [ ( ZipList.After, 4 ), ( ZipList.After, 5 ), ( ZipList.After, 6 ) ]
            ]
        , describe "update"
            [ fuzz2 int int "replaces the current item in the list" <|
                \selected new ->
                    ZipList.fromLists [ 1, 2 ] selected [ 3, 4, 5, 6 ]
                        |> ZipList.update new
                        |> expectEqualZipLists [ 1, 2 ] new [ 3, 4, 5, 6 ]
            ]
        , describe "prepend"
            [ fuzz2 (list int) (list int) "adds new items before the list" <|
                \before prepend ->
                    ZipList.fromLists before 3 [ 4, 5, 6 ]
                        |> ZipList.prepend prepend
                        |> ZipList.before
                        |> Expect.equal (prepend ++ before)
            ]
        , describe "append"
            [ fuzz2 (list int) (list int) "adds new items after the list" <|
                \after append ->
                    ZipList.fromLists [ 1, 2 ] 3 after
                        |> ZipList.append append
                        |> ZipList.after
                        |> Expect.equal (after ++ append)
            ]
        , describe "select"
            [ fuzz int "does not change if no item passes the check" <|
                \selected ->
                    ZipList.fromLists [ 1, 2 ] selected [ 4, 5, 6 ]
                        |> ZipList.select (always False)
                        |> ZipList.selected
                        |> Expect.equal selected
            , test "selects the first item to pass the check" <|
                \_ ->
                    ZipList.fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.select isEven
                        |> expectEqualZipLists [ 1 ] 2 [ 3, 4, 5, 6 ]
            ]
        , describe "rewind"
            [ fuzz int "with a singleton, is the same list" <|
                \selected ->
                    ZipList.singleton selected
                        |> ZipList.rewind
                        |> Expect.equal (ZipList.singleton selected)
            , fuzz int "selects the first item" <|
                \first ->
                    ZipList.fromLists [ first, 2 ] 3 [ 4, 5, 6 ]
                        |> ZipList.rewind
                        |> expectEqualZipLists [] first [ 2, 3, 4, 5, 6 ]
            ]
        , describe "next"
            [ fuzz int "with a singleton, is the same list" <|
                \selected ->
                    ZipList.singleton selected
                        |> ZipList.next
                        |> Expect.equal (ZipList.singleton selected)
            , fuzz2 int int "selects the next item" <|
                \selected next ->
                    ZipList.fromLists [ 1, 2 ] selected [ next, 5, 6 ]
                        |> ZipList.next
                        |> expectEqualZipLists [ 1, 2, selected ] next [ 5, 6 ]
            , fuzz int "at the end of a list, selects the last item" <|
                \last ->
                    ZipList.fromLists [ 1, 2, 3, 4, 5 ] last []
                        |> ZipList.next
                        |> expectEqualZipLists [ 1, 2, 3, 4, 5 ] last []
            ]
        , describe "loop"
            [ fuzz int "with a singleton, is the same list" <|
                \selected ->
                    ZipList.singleton selected
                        |> ZipList.loop
                        |> Expect.equal (ZipList.singleton selected)
            , fuzz2 int int "selects the next item" <|
                \selected next ->
                    ZipList.fromLists [ 1, 2 ] selected [ next, 5, 6 ]
                        |> ZipList.loop
                        |> expectEqualZipLists [ 1, 2, selected ] next [ 5, 6 ]
            , fuzz2 int (list int) "at the end of a list, selects the first item" <|
                \first before ->
                    ZipList.fromLists (first :: before) 3 []
                        |> ZipList.loop
                        |> ZipList.selected
                        |> Expect.equal first
            ]
        ]
