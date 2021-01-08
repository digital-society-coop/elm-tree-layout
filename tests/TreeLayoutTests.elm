module TreeLayoutTests exposing (testTreeLayout)

import Dict
import Expect exposing (Expectation, equal)
import Test exposing (Test, describe, test)
import TreeLayout exposing (treeLayout)


asTest : String -> Expectation -> Test
asTest label expectation =
    expectation |> always |> test label


testTreeLayout : Test
testTreeLayout =
    describe "treeLayout"
        [ [ ( "a", Nothing ) ]
            |> treeLayout 1
            |> equal (Dict.fromList [ ( "a", { x = 0, y = 1 } ) ])
            |> asTest "return coordinates for single node"
        , [ ( "a", Nothing ), ( "b", Just "a" ), ( "c", Just "a" ) ]
            |> treeLayout 1
            |> equal
                (Dict.fromList
                    [ ( "a", { x = 0, y = 1 } )
                    , ( "b", { x = -0.5, y = 2 } )
                    , ( "c", { x = 0.5, y = 2 } )
                    ]
                )
            |> asTest "return coordinates with simple tree and distance '1'"
        , [ ( "a", Nothing ), ( "b", Just "a" ), ( "c", Just "a" ) ]
            |> treeLayout 2
            |> equal
                (Dict.fromList
                    [ ( "a", { x = 0, y = 1 } )
                    , ( "b", { x = -1, y = 2 } )
                    , ( "c", { x = 1, y = 2 } )
                    ]
                )
            |> asTest "return coordinates with simple tree and distance '2'"
        , [ ( "a", Nothing )
          , ( "b", Just "a" )
          , ( "d", Just "b" )
          , ( "e", Just "b" )
          , ( "f", Just "d" )
          , ( "g", Just "d" )
          , ( "h", Just "d" )
          , ( "i", Just "d" )
          , ( "j", Just "h" )
          , ( "k", Just "h" )
          , ( "c", Just "a" )
          , ( "l", Just "c" )
          ]
            |> treeLayout 2
            |> equal
                (Dict.fromList
                    [ ( "a", { x = 0, y = 1 } )
                    , ( "b", { x = -1.5, y = 2 } )
                    , ( "c", { x = 1.5, y = 2 } )
                    , ( "d", { x = -2.5, y = 3 } )
                    , ( "e", { x = -0.5, y = 3 } )
                    , ( "f", { x = -5.5, y = 4 } )
                    , ( "g", { x = -3.5, y = 4 } )
                    , ( "h", { x = -1.5, y = 4 } )
                    , ( "i", { x = 0.5, y = 4 } )
                    , ( "j", { x = -2.5, y = 5 } )
                    , ( "k", { x = -0.5, y = 5 } )
                    , ( "l", { x = 1.5, y = 3 } )
                    ]
                )
            |> asTest "return coordinates for a complex tree"
        , []
            |> treeLayout 1
            |> equal Dict.empty
            |> asTest "return empty Dict for empty list of nodes"
        , [ ( "b", Just "a" ), ( "a", Nothing ) ]
            |> treeLayout 1
            |> equal Dict.empty
            |> asTest "return empty Dict when root is not first element"
        ]
