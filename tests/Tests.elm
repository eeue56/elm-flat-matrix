module Tests exposing (..)

import Element exposing (..)
import Test exposing (test, Test, describe)
import Expect exposing (equal)
import Array
import Matrix
import Matrix.Extra
import Html exposing (Html)


unpackMaybeSize : List (List a) -> ( Int, Int )
unpackMaybeSize ls =
    case Matrix.fromList ls of
        Just m ->
            m.size

        Nothing ->
            ( -1, -1 )



-- Impossible size test case


fromList : Test
fromList =
    describe "From list"
        [ test "equal size" <|
            \() ->
                equal ( 2, 2 ) <|
                    unpackMaybeSize [ [ 1, 1 ], [ 1, 1 ] ]
        , test "inequal size" <|
            \() ->
                equal ( 2, 3 ) <|
                    unpackMaybeSize [ [ 1, 1 ], [ 1, 1 ], [ 3, 3 ] ]
        , test "inequal size" <|
            \() ->
                equal ( 3, 2 ) <|
                    unpackMaybeSize [ [ 1, 1, 1 ], [ 1, 1, 1 ] ]
        , test "Non-consistent size" <|
            \() ->
                equal False <|
                    case Matrix.fromList [ [ 1, 1, 1 ], [ 1, 1, 1, 5 ] ] of
                        Just v ->
                            True

                        Nothing ->
                            False
        ]


repeat : Test
repeat =
    describe "Repeat"
        [ test "equal size" <|
            \() ->
                equal ( 2, 2 ) <|
                    (\z -> z.size) <|
                        Matrix.repeat 2 2 1
        , test "inequal size" <|
            \() ->
                equal ( 2, 3 ) <|
                    (\z -> z.size) <|
                        Matrix.repeat 2 3 1
        , test "inequal size matrix fromList 2x3" <|
            \() ->
                equal ( 2, 3 ) <|
                    unpackMaybeSize [ [ 1, 1 ], [ 1, 1 ], [ 1, 1 ] ]
        , test "inequal size with 1, 100" <|
            \() ->
                equal ( 1, 100 ) <|
                    (\z -> z.size) <|
                        Matrix.repeat 1 100 1
        ]


get : Test
get =
    describe "Get"
        [ test "square get first" <|
            \() ->
                equal (Just 1) <|
                    Matrix.get 0 0 <|
                        Matrix.repeat 1 1 1
        , test "non-square get middle last row 2x3" <|
            \() ->
                equal (Just 5) <|
                    Matrix.get 1 1 <|
                        Maybe.withDefault Matrix.empty <|
                            Matrix.fromList [ [ 1, 2, 3 ], [ 4, 5, 6 ] ]
        , test "non-square get invalid 2x3" <|
            \() ->
                equal (Nothing) <|
                    Matrix.get 3 1 <|
                        Maybe.withDefault Matrix.empty (Matrix.fromList [ [ 1, 2, 3 ], [ 4, 5, 6 ] ])
        , test "non-square get last 3x2" <|
            \() ->
                equal (Just 6) <|
                    Matrix.get 1 2 <|
                        Maybe.withDefault Matrix.empty (Matrix.fromList [ [ 1, 2 ], [ 3, 4 ], [ 5, 6 ] ])
        , test "non-square get last 6x2" <|
            \() ->
                equal (Just 1) <|
                    Matrix.get 0 3 <|
                        Maybe.withDefault Matrix.empty (Matrix.fromList [ [ 1, 2 ], [ 3, 4 ], [ 5, 6 ], [ 1, 2 ], [ 3, 4 ], [ 5, 6 ] ])
        , test "square get from a bigger matrix (500 x 500)" <|
            \() ->
                equal (Just 1) <|
                    Matrix.get 24 50 <|
                        Matrix.repeat 500 500 1
        , test "square get from a bigger matrix (500 x 500)" <|
            \() ->
                equal (Just 1) <|
                    Matrix.get 67 124 <|
                        Matrix.repeat 500 500 1
        , test "square get from a huge matrix (500 x 500)" <|
            \() ->
                equal (Just 567) <|
                    Matrix.get 24 50 <|
                        Matrix.set 24 50 567 <|
                            Matrix.repeat 500 500 1
        , test "square get from a huge matrix (500 x 500)" <|
            \() ->
                equal (Just 567) <|
                    Matrix.get 399 432 <|
                        Matrix.set 399 432 567 <|
                            Matrix.repeat 500 500 1
        , test "square get invalid range y too big" <|
            \() ->
                equal (Nothing) <|
                    Matrix.get 1 2 <|
                        Matrix.repeat 1 1 1
        , test "square get invalid range x too small" <|
            \() ->
                equal (Nothing) <|
                    Matrix.get -1 2 <|
                        Matrix.repeat 1 1 1
        , test "square get invalid range y too small" <|
            \() ->
                equal (Nothing) <|
                    Matrix.get 1 -2 <|
                        Matrix.repeat 1 1 1
        ]


getRow : Test
getRow =
    describe "GetRow"
        [ test "square get first row" <|
            \() ->
                equal (Just <| Array.fromList [ 2, 3 ]) <|
                    Matrix.getRow 0 <|
                        Maybe.withDefault Matrix.empty (Matrix.fromList [ [ 2, 3 ], [ 1, 1 ] ])
        , test "square get last row" <|
            \() ->
                equal (Just <| Array.fromList [ 1, 1 ]) <|
                    Matrix.getRow 1 <|
                        Maybe.withDefault Matrix.empty (Matrix.fromList [ [ 2, 3 ], [ 1, 1 ] ])
        , test "non-square get last row 2x3" <|
            \() ->
                equal (Just <| Array.fromList [ 4, 5 ]) <|
                    Matrix.getRow 2 <|
                        Maybe.withDefault Matrix.empty (Matrix.fromList [ [ 2, 3 ], [ 1, 1 ], [ 4, 5 ] ])
        , test "non-square get last row 3x2" <|
            \() ->
                equal (Just <| Array.fromList [ 1, 1, 5 ]) <|
                    Matrix.getRow 1 <|
                        Maybe.withDefault Matrix.empty (Matrix.fromList [ [ 2, 3, 4 ], [ 1, 1, 5 ] ])
        , test "square get invalid range" <|
            \() ->
                equal (Nothing) <|
                    Matrix.getRow 5 <|
                        Matrix.repeat 1 1 1
        , test "non-square get invalid range" <|
            \() ->
                equal (Nothing) <|
                    Matrix.getRow 5 <|
                        Matrix.repeat 1 5 1
        ]


getColumn : Test
getColumn =
    describe "GetColumn"
        [ test "square get first column 2x2" <|
            \() ->
                equal (Just <| Array.fromList [ 2, 1 ]) <|
                    Matrix.getColumn 0 <|
                        Maybe.withDefault Matrix.empty (Matrix.fromList [ [ 2, 3 ], [ 1, 6 ] ])
        , test "square get last column 2x2" <|
            \() ->
                equal (Just <| Array.fromList [ 3, 6 ]) <|
                    Matrix.getColumn 1 <|
                        Maybe.withDefault Matrix.empty (Matrix.fromList [ [ 2, 3 ], [ 1, 6 ] ])
        , test "non-square get first column 3x2" <|
            \() ->
                equal (Just <| Array.fromList [ 2, 1, 3 ]) <|
                    Matrix.getColumn 0 <|
                        Maybe.withDefault Matrix.empty (Matrix.fromList [ [ 2, 3 ], [ 1, 6 ], [ 3, 9 ] ])
        , test "non-square get last column 3x2" <|
            \() ->
                equal (Just <| Array.fromList [ 3, 6, 9 ]) <|
                    Matrix.getColumn 1 <|
                        Maybe.withDefault Matrix.empty (Matrix.fromList [ [ 2, 3 ], [ 1, 6 ], [ 3, 9 ] ])
        , test "non-square get first column 2x3" <|
            \() ->
                equal (Just <| Array.fromList [ 2, 1 ]) <|
                    Matrix.getColumn 0 <|
                        Maybe.withDefault Matrix.empty (Matrix.fromList [ [ 2, 3, 3 ], [ 1, 6, 9 ] ])
        , test "non-square get last column 2x3" <|
            \() ->
                equal (Just <| Array.fromList [ 3, 9 ]) <|
                    Matrix.getColumn 2 <|
                        Maybe.withDefault Matrix.empty (Matrix.fromList [ [ 2, 3, 3 ], [ 1, 6, 9 ] ])
        , test "get invalid range" <|
            \() ->
                equal (Nothing) <|
                    Matrix.getColumn 5 <|
                        Matrix.repeat 1 1 1
        ]


set : Test
set =
    describe "Set"
        [ test "Set first" <|
            \() ->
                equal (Just 5) <|
                    Matrix.get 0 0 <|
                        Matrix.set 0 0 5 <|
                            Matrix.repeat 2 2 1
        , test "Set middle 3x3" <|
            \() ->
                equal (Just 5) <|
                    Matrix.get 1 1 <|
                        Matrix.set 1 1 5 <|
                            Matrix.repeat 3 3 1
        , test "invalid set 3x3" <|
            \() ->
                equal (Matrix.repeat 2 3 1) <|
                    Matrix.set 3 1 5 <|
                        Matrix.repeat 2 3 1
        , test "Set left middle 5x3" <|
            \() ->
                equal (Just 5) <|
                    Matrix.get 3 1 <|
                        Matrix.set 3 1 5 <|
                            Matrix.repeat 5 3 1
        , test "Set outside of range does not change size" <|
            \() ->
                (\x -> equal ( 1, 1 ) x.size) <|
                    Matrix.set 5 5 1 <|
                        Matrix.repeat 1 1 1
        , test "Set with negative index does nothing" <|
            \() ->
                equal (Matrix.repeat 3 3 1) <|
                    Matrix.set (-1) 2 0 <|
                        Matrix.repeat 3 3 1
        ]


update : Test
update =
    describe "Update"
        [ test "Update first element" <|
            \() ->
                equal (Just 5) <|
                    Matrix.get 1 1 <|
                        Matrix.update 1 1 (\x -> 5) <|
                            Matrix.repeat 2 2 1
        ]


map : Test
map =
    describe "Map"
        [ test "increment every value" <|
            \() ->
                equal (Matrix.repeat 2 2 2) <|
                    Matrix.map (\x -> x + 1) <|
                        Matrix.repeat 2 2 1
        , test "identity" <|
            \() ->
                equal (Matrix.repeat 2 2 1) <|
                    Matrix.map identity <|
                        Matrix.repeat 2 2 1
        ]


map2 : Test
map2 =
    describe "Map2"
        [ test "x + y" <|
            \() ->
                equal (Just <| Matrix.repeat 2 2 2) <|
                    Matrix.map2 (\x y -> x + y) (Matrix.repeat 2 2 1) (Matrix.repeat 2 2 1)
        ]


indexedMap : Test
indexedMap =
    describe "IndexedMap"
        [ test "basic index map" <|
            \() ->
                equal (Matrix.repeat 1 1 0) <|
                    Matrix.indexedMap (\x y _ -> x + y) <|
                        Matrix.repeat 1 1 1
        , test "square (x,y) -> (x, y)" <|
            \() ->
                equal (Maybe.withDefault Matrix.empty (Matrix.fromList [ [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ) ], [ ( 0, 1 ), ( 1, 1 ), ( 2, 1 ) ], [ ( 0, 2 ), ( 1, 2 ), ( 2, 2 ) ] ])) <|
                    Matrix.indexedMap (\x y _ -> ( x, y )) <|
                        Matrix.repeat 3 3 1
        , test "non-square (x,y) -> (x, y)" <|
            \() ->
                equal (Maybe.withDefault Matrix.empty (Matrix.fromList [ [ ( 0, 0 ), ( 1, 0 ) ], [ ( 0, 1 ), ( 1, 1 ) ], [ ( 0, 2 ), ( 1, 2 ) ] ])) <|
                    Matrix.indexedMap (\x y _ -> ( x, y )) <|
                        Matrix.repeat 2 3 1
        , test "non-square (x,y) -> x + y" <|
            \() ->
                equal (Maybe.withDefault Matrix.empty (Matrix.fromList [ [ 0, 1 ], [ 1, 2 ], [ 2, 3 ] ])) <|
                    Matrix.indexedMap (\x y _ -> x + y) <|
                        Matrix.repeat 2 3 1
        , test "non-square (x,y) -> (x, y)" <|
            \() ->
                equal (Maybe.withDefault Matrix.empty (Matrix.fromList [ [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ) ], [ ( 0, 1 ), ( 1, 1 ), ( 2, 1 ) ] ])) <|
                    Matrix.indexedMap (\x y _ -> ( x, y )) <|
                        Matrix.repeat 3 2 1
        ]


toIndexedArray : Test
toIndexedArray =
    let
        twoByThreeXAndY : List ( ( Int, Int ), Int )
        twoByThreeXAndY =
            [ ( ( 0, 0 ), 0 )
            , ( ( 1, 0 ), 1 )
            , ( ( 0, 1 ), 1 )
            , ( ( 1, 1 ), 2 )
            , ( ( 0, 2 ), 2 )
            , ( ( 1, 2 ), 3 )
            ]

        threeByThreeXAndY =
            [ ( ( 0, 0 ), 0 )
            , ( ( 1, 0 ), 1 )
            , ( ( 2, 0 ), 2 )
            , ( ( 0, 1 ), 1 )
            , ( ( 1, 1 ), 2 )
            , ( ( 2, 1 ), 3 )
            , ( ( 0, 2 ), 2 )
            , ( ( 1, 2 ), 3 )
            , ( ( 2, 2 ), 4 )
            ]

        twoByThreeOnes =
            [ ( ( 0, 0 ), 1 )
            , ( ( 1, 0 ), 1 )
            , ( ( 0, 1 ), 1 )
            , ( ( 1, 1 ), 1 )
            , ( ( 0, 2 ), 1 )
            , ( ( 1, 2 ), 1 )
            ]
    in
        describe "toIndexedArray"
            [ test "(x,y) -> ((x, y), x + y) for non-square 2x3" <|
                \() ->
                    equal twoByThreeXAndY <|
                        Array.toList <|
                            Matrix.toIndexedArray <|
                                Matrix.indexedMap (\x y _ -> x + y) <|
                                    Matrix.repeat 2 3 1
            , test "(x,y) -> ((x, y), x + y) for non-square 2x3" <|
                \() ->
                    equal twoByThreeOnes <|
                        Array.toList <|
                            Matrix.toIndexedArray <|
                                Matrix.repeat 2 3 1
            , test "(x,y) -> ((x, y), x + y) for square 3x3" <|
                \() ->
                    equal threeByThreeXAndY <|
                        Array.toList <|
                            Matrix.toIndexedArray <|
                                Matrix.indexedMap (\x y _ -> x + y) <|
                                    Matrix.repeat 3 3 1
            ]


filter : Test
filter =
    describe "Filter"
        [ test "Keep ones" <|
            \() ->
                equal (Array.repeat 2 1) <|
                    Matrix.filter (\x -> x == 1) <|
                        Maybe.withDefault Matrix.empty <|
                            Matrix.fromList [ [ 2, 3 ], [ 1, 1 ] ]
        ]


concatVertical : Test
concatVertical =
    describe "Append row"
        [ test "add two rows to the end" <|
            \() ->
                equal (Just <| Matrix.repeat 1 4 2) <|
                    Matrix.concatVertical (Matrix.repeat 1 2 2) (Matrix.repeat 1 2 2)
        ]


concatHorizontal : Test
concatHorizontal =
    describe "Append column"
        [ test "add a column to the end" <|
            \() ->
                equal (Just <| Matrix.repeat 2 2 2) <|
                    Matrix.concatHorizontal (Matrix.repeat 1 2 2) (Matrix.repeat 1 2 2)
        ]


add : Test
add =
    describe "Add"
        [ test "Add uniform square matricies" <|
            \() ->
                equal (Just <| Matrix.repeat 2 2 3) <|
                    Matrix.Extra.add (Matrix.repeat 2 2 1) (Matrix.repeat 2 2 2)
        , test "Add non-uniform square matricies" <|
            \() ->
                equal (Matrix.fromList [ [ 3, 4 ], [ 4, 5 ] ]) <|
                    Matrix.Extra.add (Maybe.withDefault Matrix.empty (Matrix.fromList [ [ 1, 2 ], [ 2, 3 ] ])) (Matrix.repeat 2 2 2)
        , test "Can't add two differently sized matricies" <|
            \() ->
                equal Nothing <|
                    Matrix.Extra.add (Matrix.repeat 2 2 1) (Matrix.repeat 2 3 2)
        ]


subtract : Test
subtract =
    describe "Subtract"
        [ test "Subtract uniform square matricies" <|
            \() ->
                equal (Just <| Matrix.repeat 2 2 0) <|
                    Matrix.Extra.subtract (Matrix.repeat 2 2 1) (Matrix.repeat 2 2 1)
        , test "Subtract non-uniform square matricies" <|
            \() ->
                equal (Matrix.fromList [ [ -1, 0 ], [ 0, 1 ] ]) <|
                    Matrix.Extra.subtract (Maybe.withDefault Matrix.empty (Matrix.fromList [ [ 1, 2 ], [ 2, 3 ] ])) (Matrix.repeat 2 2 2)
        , test "Can't subtract two differently sized matricies" <|
            \() ->
                equal Nothing <|
                    Matrix.Extra.subtract (Matrix.repeat 2 2 1) (Matrix.repeat 2 3 2)
        ]


hadamard : Test
hadamard =
    describe "Hadamard"
        [ test "Hadamard uniform square matricies" <|
            \() ->
                equal (Just <| Matrix.repeat 2 2 1) <|
                    Matrix.Extra.hadamard (Matrix.repeat 2 2 1) (Matrix.repeat 2 2 1)
        , test "Hadamard non-uniform square matricies" <|
            \() ->
                equal (Matrix.fromList [ [ 2, 4 ], [ 4, 6 ] ]) <|
                    Matrix.Extra.hadamard (Maybe.withDefault Matrix.empty (Matrix.fromList [ [ 1, 2 ], [ 2, 3 ] ])) (Matrix.repeat 2 2 2)
        , test "Can't hadamard two differently sized matricies" <|
            \() ->
                equal Nothing <|
                    Matrix.Extra.hadamard (Matrix.repeat 2 2 1) (Matrix.repeat 2 3 2)
        ]


power : Test
power =
    describe "power"
        [ test "power uniform square matricies" <|
            \() ->
                equal (Just <| Matrix.repeat 2 2 1) <|
                    Matrix.Extra.power (Matrix.repeat 2 2 1) (Matrix.repeat 2 2 1)
        , test "power non-uniform square matricies" <|
            \() ->
                equal (Matrix.fromList [ [ 1, 4 ], [ 4, 9 ] ]) <|
                    Matrix.Extra.power (Maybe.withDefault Matrix.empty (Matrix.fromList [ [ 1, 2 ], [ 2, 3 ] ])) (Matrix.repeat 2 2 2)
        , test "Can't power two differently sized matricies" <|
            \() ->
                equal Nothing <|
                    Matrix.Extra.power (Matrix.repeat 2 2 1) (Matrix.repeat 2 3 2)
        ]


neighbours : Test
neighbours =
    describe "neighbours"
        [ test "neighbours of square matrix" <|
            \() ->
                equal 8 <|
                    List.sum <|
                        Matrix.Extra.neighbours 1 1 <|
                            Matrix.repeat 3 3 1
        , test "neighbours of square matrix in the bottom middle" <|
            \() ->
                equal 5 <|
                    List.sum <|
                        Matrix.Extra.neighbours 0 1 <|
                            Matrix.repeat 3 3 1
        , test "neighbours of square matrix in the bottom middle" <|
            \() ->
                equal [ 1, 1, 1 ] <|
                    Matrix.Extra.neighbours 0 0 <|
                        Matrix.repeat 3 3 1
        ]


all : Test
all =
    describe "Tests"
        [ get
        , getRow
        , getColumn
        , concatVertical
        , concatHorizontal
        , set
        , update
        , fromList
        , repeat
        , map
        , map2
        , filter
        , indexedMap
        , toIndexedArray
        , add
        , subtract
        , hadamard
        , power
        , neighbours
        ]
