module ZipList exposing
    ( ZipList, fromLists, singleton
    , toList, before, selected, after
    , next, rewind, select
    , map, append, prepend, update
    )

{-| A non-empty list.

@docs ZipList, fromLists, singleton

# Reading

@docs toList, before, selected, after


# Traversal

@docs next, rewind, select


# Updating

@docs map, append, prepend, update

-}


{-| a ZipList
-}
type ZipList a
    = ZipList
        { previous : List a
        , current : a
        , remaining : List a
        }


{-| a ZipList

    .fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
        |> .toList
        == [ 1, 2, 3, 4, 5, 6 ]

-}
fromLists : List a -> a -> List a -> ZipList a
fromLists previous current remaining =
    ZipList { previous = previous, current = current, remaining = remaining }


{-| a ZipList with a single item

    .singleton 1
        |> .toList
        == [ 1 ]

-}
singleton : a -> ZipList a
singleton item =
    fromLists [] item []


{-| a list of all items

    .fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
        |> .toList
        == [ 1, 2, 3, 4, 5, 6 ]

-}
toList : ZipList a -> List a
toList (ZipList list) =
    list.previous ++ list.current :: list.remaining


{-| a list of all items before current

    .fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
        |> .before
        == [ 1, 2 ]

-}
before : ZipList a -> List a
before (ZipList list) =
    list.previous


{-| the current item

    .fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
        |> .selected
        == 3

-}
selected : ZipList a -> a
selected (ZipList list) =
    list.current


{-| a list of all items after current

    .fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
        |> .after
        == [ 4, 5, 6 ]

-}
after : ZipList a -> List a
after (ZipList list) =
    list.remaining


{-| transform each element of the list

    .fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
        |> .map (\n -> n * 2)
        |> .toList
        == [ 2, 4, 6, 8, 10, 12 ]

-}
map : (a -> b) -> ZipList a -> ZipList b
map fn (ZipList list) =
    fromLists
        (List.map fn list.previous)
        (fn list.current)
        (List.map fn list.remaining)


{-| check each value in the list select the first to be True

    .fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
        |> .select isEvan
        |> .selected
        == 2

-}
select : (a -> Bool) -> ZipList a -> ZipList a
select check zipList =
    case selectHelp check [] (toList zipList) of
        Nothing ->
            zipList

        Just newZipList ->
            newZipList


selectHelp : (a -> Bool) -> List a -> List a -> Maybe (ZipList a)
selectHelp check checked remaining =
    case remaining of
        [] ->
            Nothing

        first :: lastItems ->
            if check first then
                Just (fromLists checked first lastItems)

            else
                selectHelp check (checked ++ [ first ]) lastItems


{-| prepend list before the ZipList

    .fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
        |> .prepend [ 7, 8 ]
        |> .previous
        == [ 7, 8, 1, 2 ]

-}
prepend : List a -> ZipList a -> ZipList a
prepend newItems (ZipList list) =
    fromLists
        (newItems ++ list.previous)
        list.current
        list.remaining


{-| append list after the ZipList

    .fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
        |> .append [ 7, 8 ]
        |> .after
        == [ 4, 5, 6, 7, 8 ]

-}
append : List a -> ZipList a -> ZipList a
append newItems (ZipList list) =
    fromLists
        list.previous
        list.current
        (list.remaining ++ newItems)


{-| replace the current item

    .fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
        |> .update 9
        |> .current
        == 9

-}
update : a -> ZipList a -> ZipList a
update newItem (ZipList list) =
    fromLists list.previous newItem list.remaining


{-| select the first item

    .fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
        |> .rewind
        |> .selected
        == 1

-}
rewind : ZipList a -> ZipList a
rewind ((ZipList list) as original) =
    case list.previous of
        [] ->
            original

        first :: otherItems ->
            fromLists [] first (otherItems ++ list.current :: list.remaining)


{-| select the next item, when the list is at the end it will rewind

    .fromLists [ 1, 2 ] 3 [ 4, 5, 6 ]
        |> .next
        |> .selected
        == 4

-}
next : ZipList a -> ZipList a
next ((ZipList list) as original) =
    case list.remaining of
        [] ->
            rewind original

        nextItem :: otherItems ->
            fromLists (list.previous ++ [ list.current ]) nextItem otherItems
