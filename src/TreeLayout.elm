module TreeLayout exposing (treeLayout)

{-| Return x and y coordinates for a general tree, layed out according to aesthetic rules
defined by [(Walker, 1989)](http://www.cs.unc.edu/techreports/89-034.pdf). This implementation
is based on the pseudo-code given in the improved algorithm that runs in linear time by
[(Buchheim, Junger and Leipert, 2006)](http://dirk.jivas.de/papers/buchheim02improving.pdf).

While providing the coordinates, this module does not render the tree to keep it generic.

@docs treeLayout

-}

import Canopy
import Canopy.Extra exposing (areSiblings, childrenValues, getLevel, leftMostSibling, leftSibling)
import Dict exposing (Dict)



-- Types


type alias Coordinate =
    { x : Float
    , y : Float
    }



-- Model


type alias Model comparable =
    { tree : Canopy.Node comparable
    , positions : Dict comparable (Position comparable)
    , distance : Float
    }


{-| Initializes the model representation for the algorithm.
-}
initModel : Float -> Canopy.Node comparable -> Model comparable
initModel distance tree =
    let
        initPositions number node =
            let
                value =
                    Canopy.value node
            in
            ( value, initPosition value number ) :: List.concat (List.indexedMap initPositions (Canopy.children node))
    in
    { tree = tree
    , positions = Dict.fromList <| initPositions 0 tree
    , distance = distance
    }


getPosition : comparable -> Model comparable -> Position comparable
getPosition value model =
    Dict.get value model.positions
        |> Maybe.withDefault (initPosition value 0)


updatePosition : comparable -> Position comparable -> Model comparable -> Model comparable
updatePosition value position model =
    { model
        | positions = Dict.insert value position model.positions
    }


getPrelim : Model comparable -> comparable -> Float
getPrelim model value =
    (getPosition value model).prelim


getMod : Model comparable -> Maybe comparable -> Float
getMod model maybeValue =
    maybeValue
        |> Maybe.map (\v -> (getPosition v model).mod)
        |> Maybe.withDefault 0


type alias Position comparable =
    { prelim : Float
    , ancestor : comparable
    , mod : Float
    , thread : Maybe comparable
    , change : Float
    , shift : Float
    , number : Int
    }


initPosition : comparable -> Int -> Position comparable
initPosition value number =
    { prelim = 0
    , ancestor = value
    , mod = 0
    , thread = Nothing
    , change = 0
    , shift = 0
    , number = number
    }


type alias Contour comparable =
    { inRight : Maybe comparable
    , inLeft : Maybe comparable
    , outLeft : Maybe comparable
    , outRight : Maybe comparable
    , modInRight : Float
    , modInLeft : Float
    , modOutLeft : Float
    , modOutRight : Float
    }



-- Functions


{-| Return a dictionary of x and y coordinates keyed by a unique identifier for a node for a
given 'distance' (used as a constant for x spacing) and 'nodes' (a list of hierarchy descriptors
starting with the root, represented by tuples of node identifier and parent node identifier).

    treeLayout 2 [ ( "a", Nothing ), ( "b", Just "a" ), ( "c", Just "a" ) ]
    --> (Dict.fromList [ ( "a", { x = 0, y = 1 } ), ( "b", { x = -1, y = 2 } ), ( "c", { x = 1, y = 2 } ) ])

**Note:** The x coordinates will have negative values, as the root node will always be at 0.
The y coordinates will be integers from 1 to the max depth of the tree.

-}
treeLayout : Float -> List ( comparable, Maybe comparable ) -> Dict comparable Coordinate
treeLayout distance nodes =
    let
        algorithm tree =
            let
                root =
                    Canopy.value tree
            in
            initModel distance tree
                |> firstWalk root
                |> (\model -> secondWalk ( root, -1 * getPrelim model root ) Dict.empty model)
    in
    Canopy.fromList nodes
        |> Maybe.map algorithm
        |> Maybe.withDefault Dict.empty


{-| Compute a preliminary x-coordinate for node 'value'.
-}
firstWalk : comparable -> Model comparable -> Model comparable
firstWalk value model =
    let
        position =
            getPosition value model

        maybeLeftSibling =
            leftSibling value model.tree
    in
    case childrenValues value model.tree of
        [] ->
            case maybeLeftSibling of
                Just lSibling ->
                    let
                        lSiblingPrelim =
                            getPrelim model lSibling
                    in
                    updatePosition value { position | prelim = lSiblingPrelim + model.distance } model

                Nothing ->
                    updatePosition value { position | prelim = 0 } model

        (leftMostChild :: _) as children ->
            let
                rightMostChild =
                    children
                        |> List.reverse
                        |> List.head
                        |> Maybe.withDefault leftMostChild

                walkAndApport : comparable -> ( comparable, Model comparable ) -> ( comparable, Model comparable )
                walkAndApport value_ ( defaultAncestor, model_ ) =
                    firstWalk value_ model_
                        |> apportion ( value_, defaultAncestor )

                walkAndApportChildren : Model comparable -> Model comparable
                walkAndApportChildren model_ =
                    List.foldl walkAndApport ( leftMostChild, model_ ) children
                        |> Tuple.second

                updateModifiers : Model comparable -> Model comparable
                updateModifiers model_ =
                    let
                        midpoint =
                            0.5 * (getPrelim model_ leftMostChild + getPrelim model_ rightMostChild)

                        position_ =
                            getPosition value model_
                    in
                    case maybeLeftSibling of
                        Just lSibling ->
                            let
                                lSiblingPrelim =
                                    getPrelim model_ lSibling

                                prelim =
                                    lSiblingPrelim + model_.distance
                            in
                            updatePosition value
                                { position_
                                    | prelim = prelim
                                    , mod = prelim - midpoint
                                }
                                model_

                        Nothing ->
                            updatePosition value { position_ | prelim = midpoint } model_
            in
            walkAndApportChildren model
                |> executeShifts value
                |> updateModifiers


{-| Combines the new subtree with the previous subtrees.
-}
apportion : ( comparable, comparable ) -> Model comparable -> ( comparable, Model comparable )
apportion ( value, defaultAncestor ) model =
    let
        initContour : comparable -> Model comparable -> ( Contour comparable, Model comparable )
        initContour lSibling model_ =
            let
                inRight =
                    Just value

                inLeft =
                    Just lSibling

                outLeft =
                    leftMostSibling value model_.tree

                outRight =
                    Just value
            in
            ( { inRight = inRight
              , inLeft = inLeft
              , outLeft = outLeft
              , outRight = outRight
              , modInRight = getMod model_ inRight
              , modInLeft = getMod model_ inLeft
              , modOutLeft = getMod model_ outLeft
              , modOutRight = getMod model_ outRight
              }
            , model_
            )

        updateNextOutRight : comparable -> ( Contour comparable, Model comparable ) -> ( Contour comparable, Model comparable )
        updateNextOutRight nextOutRight ( contour_, model_ ) =
            let
                nextOutRightPosition =
                    getPosition nextOutRight model_
            in
            ( contour_
            , updatePosition nextOutRight { nextOutRightPosition | ancestor = value } model_
            )

        checkShiftAndMove : comparable -> comparable -> ( Contour comparable, Model comparable ) -> ( Contour comparable, Model comparable )
        checkShiftAndMove nextInLeft nextInRight ( contour_, model_ ) =
            let
                nextInLeftPosition =
                    getPosition nextInLeft model_

                nextInRightPosition =
                    getPosition nextInRight model_

                shift =
                    (nextInLeftPosition.prelim + contour_.modInLeft) - (nextInRightPosition.prelim + contour_.modInRight) + model_.distance
            in
            if shift > 0 then
                ( { contour_
                    | modInRight = contour_.modInRight + shift
                    , modOutRight = contour_.modOutRight + shift
                  }
                , moveSubtree ( ancestor ( nextInLeft, value, defaultAncestor ) model_, value, shift ) model_
                )

            else
                ( contour_, model_ )

        updateContour : Maybe comparable -> Maybe comparable -> ( Contour comparable, Model comparable ) -> ( Contour comparable, Model comparable )
        updateContour maybeNextInLeft maybeNextInRight ( contour_, model_ ) =
            let
                maybeNextOutLeft =
                    contour_.outLeft
                        |> Maybe.andThen (nextLeft model_)

                maybeNextOutRight =
                    contour_.outRight
                        |> Maybe.andThen (nextRight model_)
            in
            ( { modInLeft = contour_.modInLeft + getMod model_ maybeNextInLeft
              , modInRight = contour_.modInRight + getMod model_ maybeNextInRight
              , modOutLeft = contour_.modOutLeft + getMod model_ maybeNextOutLeft
              , modOutRight = contour_.modOutRight + getMod model_ maybeNextOutRight
              , inLeft = maybeNextInLeft
              , inRight = maybeNextInRight
              , outLeft = maybeNextOutLeft
              , outRight = maybeNextOutRight
              }
            , model_
            )

        moveSubtreeWhileLoop : ( Contour comparable, Model comparable ) -> ( Contour comparable, Model comparable )
        moveSubtreeWhileLoop ( contour_, model_ ) =
            let
                maybeNextInLeft =
                    contour_.inLeft
                        |> Maybe.andThen (nextRight model_)

                maybeNextInRight =
                    contour_.inRight
                        |> Maybe.andThen (nextLeft model_)

                maybeNextOutRight =
                    contour_.outRight
                        |> Maybe.andThen (nextRight model_)
            in
            case ( maybeNextInLeft, maybeNextInRight, maybeNextOutRight ) of
                ( Just nextInLeft, Just nextInRight, Just nextOutRight ) ->
                    updateNextOutRight nextOutRight ( contour_, model_ )
                        |> checkShiftAndMove nextInLeft nextInRight
                        |> updateContour maybeNextInLeft maybeNextInRight
                        |> moveSubtreeWhileLoop

                _ ->
                    ( contour_, model_ )

        updateOutRight : ( Contour comparable, Model comparable ) -> ( Contour comparable, Model comparable )
        updateOutRight ( contour_, model_ ) =
            let
                maybeNextInLeft_ =
                    contour_.inLeft
                        |> Maybe.andThen (nextRight model_)

                maybeNextOutRight_ =
                    contour_.outRight
                        |> Maybe.andThen (nextRight model_)
            in
            case ( maybeNextInLeft_, maybeNextOutRight_, contour_.outRight ) of
                ( Just _, Nothing, Just outRight ) ->
                    let
                        outRightPosition =
                            getPosition outRight model_
                    in
                    ( contour_
                    , updatePosition outRight
                        { outRightPosition
                            | thread = maybeNextInLeft_
                            , mod = outRightPosition.mod + contour_.modInLeft - contour_.modOutRight
                        }
                        model_
                    )

                _ ->
                    ( contour_, model_ )

        updateOutLeft : ( Contour comparable, Model comparable ) -> ( comparable, Model comparable )
        updateOutLeft ( contour_, model_ ) =
            let
                maybeNextInRight_ =
                    contour_.inRight
                        |> Maybe.andThen (nextLeft model_)

                maybeNextOutLeft_ =
                    contour_.outLeft
                        |> Maybe.andThen (nextLeft model_)
            in
            case ( maybeNextInRight_, maybeNextOutLeft_, contour_.outLeft ) of
                ( Just _, Nothing, Just outLeft ) ->
                    let
                        outLeftPosition =
                            getPosition outLeft model_
                    in
                    ( value
                    , updatePosition outLeft
                        { outLeftPosition
                            | thread = maybeNextInRight_
                            , mod = outLeftPosition.mod + contour_.modInRight - contour_.modOutLeft
                        }
                        model_
                    )

                _ ->
                    ( defaultAncestor, model_ )
    in
    case leftSibling value model.tree of
        Just lSibling ->
            initContour lSibling model
                |> moveSubtreeWhileLoop
                |> updateOutRight
                |> updateOutLeft

        _ ->
            ( defaultAncestor, model )


{-| Returns x (by summing up modifiers recursively) and y (by getting the level) coordinates.
-}
secondWalk : ( comparable, Float ) -> Dict comparable Coordinate -> Model comparable -> Dict comparable Coordinate
secondWalk ( value, mod ) coordinates model =
    let
        children =
            childrenValues value model.tree

        position =
            getPosition value model

        sumModifiers coordinates_ =
            Dict.insert value { x = position.prelim + mod, y = getLevel value model.tree |> toFloat } coordinates_

        secondWalkChildren child acc =
            secondWalk ( child, mod + position.mod ) acc model
    in
    List.foldl secondWalkChildren (sumModifiers coordinates) children


{-| Executes the shifts computed in moveSubtree.
-}
executeShifts : comparable -> Model comparable -> Model comparable
executeShifts value model =
    let
        children =
            childrenValues value model.tree
                |> List.reverse

        executeShift : ( ( Float, Float ), List comparable ) -> Model comparable -> Model comparable
        executeShift ( ( shift, change ), children_ ) model_ =
            case children_ of
                [] ->
                    model_

                current :: rest ->
                    let
                        position =
                            getPosition current model_

                        updatedChange =
                            change + position.change

                        updatedShift =
                            shift + position.shift + updatedChange
                    in
                    updatePosition current
                        { position
                            | prelim = position.prelim + shift
                            , mod = position.mod + shift
                        }
                        model_
                        |> executeShift ( ( updatedShift, updatedChange ), rest )
    in
    executeShift ( ( 0, 0 ), children ) model


{-| Returns left of the greatest uncommon ancestors of 'left' and its 'right' neighbour.
-}
ancestor : ( comparable, comparable, comparable ) -> Model comparable -> comparable
ancestor ( left, right, defaultAncestor ) model =
    let
        inLeftAncestor =
            (getPosition left model).ancestor
    in
    if areSiblings inLeftAncestor right model.tree then
        inLeftAncestor

    else
        defaultAncestor


{-| Move the subtree rooted at 'right'.
-}
moveSubtree : ( comparable, comparable, Float ) -> Model comparable -> Model comparable
moveSubtree ( left, right, shift ) model =
    let
        leftPosition =
            getPosition left model

        rightPosition0 =
            getPosition right model

        subtrees =
            toFloat <| rightPosition0.number - leftPosition.number

        updateLeftPosition model_ =
            updatePosition left { leftPosition | change = leftPosition.change + (shift / subtrees) } model_

        rightPosition1 =
            { rightPosition0
                | change = rightPosition0.change - (shift / subtrees)
                , shift = rightPosition0.shift + shift
                , prelim = rightPosition0.prelim + shift
                , mod = rightPosition0.mod + shift
            }
    in
    updatePosition right rightPosition1 model
        |> updateLeftPosition


{-| Return the leftmost child of 'value', otherwise the thread of 'value'.
-}
nextLeft : Model comparable -> comparable -> Maybe comparable
nextLeft model value =
    case childrenValues value model.tree of
        [] ->
            let
                position =
                    getPosition value model
            in
            position.thread

        child :: _ ->
            Just child


{-| Return the rightmost child of 'value', otherwise the thread of 'value'.
-}
nextRight : Model comparable -> comparable -> Maybe comparable
nextRight model value =
    case childrenValues value model.tree |> List.reverse of
        [] ->
            let
                position =
                    getPosition value model
            in
            position.thread

        child :: _ ->
            Just child
