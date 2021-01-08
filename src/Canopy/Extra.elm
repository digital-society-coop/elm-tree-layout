module Canopy.Extra exposing (areSiblings, childrenValues, getLevel, leftMostSibling, leftSibling)

import Canopy exposing (Node, children, get, parent, path, siblings, value)


{-| Check that the target values are siblings.

    node "root"
        [ leaf "qux"
        , leaf "bar"
        , leaf "baz"
        ]
        |> areSiblings "bar" "baz"
    --> True

    node "root"
        [ leaf "qux"
        , leaf "bar"
        , leaf "baz"
        ]
        |> areSiblings "root" "baz"
    --> False

-}
areSiblings : a -> a -> Node a -> Bool
areSiblings target1 target2 tree =
    siblings target1 tree
        |> List.map value
        |> List.member target2


{-| Retrieve the values of children for the target value.

    node "root"
        [ leaf "qux"
        , node "foo"
            [ leaf "bar"
            , leaf "baz"
            ]
        ]
        |> childrenValues "foo"
    --> [ "bar", "baz" ]

-}
childrenValues : a -> Node a -> List a
childrenValues target tree =
    get target tree
        |> Maybe.map (children >> List.map value)
        |> Maybe.withDefault []


{-| Get the level of the target value or 0 if value is not in tree.

    node "root"
        [ leaf "qux"
        , leaf "bar"
        , leaf "baz"
        ]
        |> getLevel "bar"
    --> 2

    node "root"
        [ leaf "qux"
        , leaf "bar"
        , leaf "baz"
        ]
        |> getLevel "root"
    --> 1

    node "root"
        [ leaf "qux"
        , leaf "bar"
        , leaf "baz"
        ]
        |> getLevel "foo"
    --> 0

-}
getLevel : a -> Node a -> Int
getLevel target tree =
    path target tree
        |> List.length


{-| Get the left most sibling of the target value.

    node "root"
        [ leaf "qux"
        , leaf "bar"
        , leaf "baz"
        ]
        |> leftMostSibling "baz"
    --> Just "qux"

-}
leftMostSibling : a -> Node a -> Maybe a
leftMostSibling target tree =
    case siblings target tree of
        [] ->
            Nothing

        leftMost :: _ ->
            Just (value leftMost)


{-| Get the left sibling of the target value.

    node "root"
        [ leaf "qux"
        , leaf "bar"
        , leaf "baz"
        ]
        |> leftSibling "baz"
    --> Just "bar"

-}
leftSibling : a -> Node a -> Maybe a
leftSibling target tree =
    let
        siblingsInclusive : a -> List (Node a)
        siblingsInclusive target_ =
            parent target_ tree
                |> Maybe.map children
                |> Maybe.withDefault []

        nextSibling : a -> List (Node a) -> Maybe (Node a)
        nextSibling target_ siblings_ =
            case siblings_ of
                [] ->
                    Nothing

                current :: rest ->
                    if value current == target_ then
                        List.head rest

                    else
                        nextSibling target_ rest
    in
    siblingsInclusive target
        |> List.reverse
        |> nextSibling target
        |> Maybe.map value
