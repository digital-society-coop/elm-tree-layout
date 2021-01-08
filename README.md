# elm-tree-layout
Elm implementation of Buchheim, Junger and Leipert's tree layout algorithm.

This library does not render the tree, so that the rendering logic could be kept separate.
Given a 'distance' (used as a constant for x spacing) and 'nodes' (list of hierarchy descriptors,
which are tuples of node identifier and parent node identifier, starting with the root), it
returns a dictionary of x and y coordinates keyed by the unique identifier of a node:
```elm
 [ ( "a", Nothing )
 , ( "b", Just "a" )
 , ( "c", Just "a" )
 ]
     |> TreeLayout.treeLayout 2
 --> (Dict.fromList
         [ ( "a", { x = 0, y = 1 } )
         , ( "b", { x = -1, y = 2 } )
         , ( "c", { x = 1, y = 2 } )
         ]
     )
```
The pseude code written by
[(Buchheim, Junger and Leipert, 2006)](http://dirk.jivas.de/papers/buchheim02improving.pdf)
implements aesthetic rules defined by [(Walker, 1989)](http://www.cs.unc.edu/techreports/89-034.pdf).
in a linear time for trees of unbounded degree.
