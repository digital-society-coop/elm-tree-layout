module Canopy.ExtraTests exposing (testAreSiblings, testChildrenValues, testGetLevel, testLeftMostSibling, testLeftSibling)

import Canopy exposing (leaf, node)
import Canopy.Extra exposing (areSiblings, childrenValues, getLevel, leftMostSibling, leftSibling)
import Expect exposing (Expectation, equal)
import Test exposing (Test, describe, test)


asTest : String -> Expectation -> Test
asTest label expectation =
    expectation |> always |> test label


testAreSiblings : Test
testAreSiblings =
    describe "areSiblings"
        [ node "a" [ leaf "b", leaf "c" ]
            |> areSiblings "b" "c"
            |> equal True
            |> asTest "return True when are siblings"
        , node "a" [ leaf "b", leaf "c" ]
            |> areSiblings "a" "c"
            |> equal False
            |> asTest "return False when are not siblings"
        , node "a" [ leaf "b", leaf "c" ]
            |> areSiblings "a" "d"
            |> equal False
            |> asTest "return False when one target is not in tree"
        ]


testChildrenValues : Test
testChildrenValues =
    describe "childrenValues"
        [ node "a" [ leaf "b", leaf "c" ]
            |> childrenValues "a"
            |> equal [ "b", "c" ]
            |> asTest "return children values when target is present"
        , node "a" [ leaf "b", leaf "c" ]
            |> childrenValues "b"
            |> equal []
            |> asTest "return empty List when target is leaf"
        , node "a" [ leaf "b", leaf "c" ]
            |> childrenValues "d"
            |> equal []
            |> asTest "return empty List when target is not in tree"
        ]


testGetLevel : Test
testGetLevel =
    describe "getLevel"
        [ node "a" [ leaf "b", leaf "c" ]
            |> getLevel "a"
            |> equal 1
            |> asTest "return 1 for root"
        , node "a" [ leaf "b", leaf "c" ]
            |> getLevel "b"
            |> equal 2
            |> asTest "return correct level"
        , node "a" [ leaf "b", leaf "c" ]
            |> getLevel "d"
            |> equal 0
            |> asTest "return 0 for target not in tree"
        ]


testLeftMostSibling : Test
testLeftMostSibling =
    describe "leftMostSibling"
        [ node "a" [ leaf "b", leaf "c", leaf "d" ]
            |> leftMostSibling "a"
            |> equal Nothing
            |> asTest "return Nothing for no siblings"
        , node "a" [ leaf "b", leaf "c", leaf "d" ]
            |> leftMostSibling "c"
            |> equal (Just "b")
            |> asTest "return left most sibling for next sibling"
        , node "a" [ leaf "b", leaf "c", leaf "d" ]
            |> leftMostSibling "d"
            |> equal (Just "b")
            |> asTest "return left most sibling for further sibling"
        , node "a" [ leaf "b", leaf "c", leaf "d" ]
            |> leftMostSibling "e"
            |> equal Nothing
            |> asTest "return Nothing for target not in tree"
        ]


testLeftSibling : Test
testLeftSibling =
    describe "leftSibling"
        [ node "a" [ leaf "b", leaf "c", leaf "d" ]
            |> leftSibling "a"
            |> equal Nothing
            |> asTest "return Nothing for no siblings"
        , node "a" [ leaf "b", leaf "c", leaf "d" ]
            |> leftSibling "b"
            |> equal Nothing
            |> asTest "return Nothing for left most sibling"
        , node "a" [ leaf "b", leaf "c", leaf "d" ]
            |> leftSibling "c"
            |> equal (Just "b")
            |> asTest "return left sibling for middle sibling"
        , node "a" [ leaf "b", leaf "c", leaf "d" ]
            |> leftSibling "d"
            |> equal (Just "c")
            |> asTest "return left sibling for rightmost sibling"
        , node "a" [ leaf "b", leaf "c", leaf "d" ]
            |> leftSibling "e"
            |> equal Nothing
            |> asTest "return Nothing for target not in tree"
        ]
