module Matrix.Extra (
    add, subtract, 
    hadamard, (.*),
    power, (.^),
    prettyPrint,
    neighbours, diagonals, indexedNeighbours, neighboursFour) where
{-| Extra methods for Matricies

# Element-wise computation
@docs add, subtract, hadamard, power

# Syntax aliases
@docs (.*), (.^) 

# Interacting with other cells
@docs neighbours, indexedNeighbours, diagonals, neighboursFour

@docs prettyPrint

-}
import Matrix exposing (Matrix, map2, get, getRow, height, width)
import Array exposing (fromList, toList)
import Html exposing (Html, table, tr, td, fromElement)
import Html.Attributes exposing (style)
import Graphics.Element exposing (show)

-- Helper function for unpacking lists
unpackMaybeList : (a -> Maybe b) -> List a -> List b
unpackMaybeList fn ls =
  List.foldl (\item ls ->
    case fn item of
      Just it -> ls ++ [it]
      Nothing -> ls
    ) [] ls

{-|
Print out a matrix into a table
-}
prettyPrint : Matrix a -> Html
prettyPrint matrix =
  let
    printXIndex = tr [] (td [style [("background-color", "black") ]] [] :: List.map printXCell [0..(width matrix)-1] ) 
    printXCell cell = 
        td [style [ ("border", "1px solid black" ), ("background-color", "#A8A8F5") ] ]
          <| [fromElement <| show cell]
    printCell cell = 
      td [style [ ("border", "1px solid black" ) ] ] 
        <| [fromElement <| show cell]
    printRow i row =
        tr [] 
          <| (printXCell i) :: (Array.toList <| Array.map printCell row)
  in
    table [] 
      <| printXIndex
      :: (List.indexedMap printRow
      <| unpackMaybeList (\i -> getRow i matrix) [0..(height matrix)-1])


{-|
Get the neighbours of a point (x, y) in the matrix
If on edge, then no wrapping happens - they are excluded
-}
neighbours : Int -> Int -> Matrix a  -> List a
neighbours x y matrix = 
  let 
    grab di dj =
      Matrix.get (x + di) (y + dj) matrix 
  in
    unpackMaybeList identity 
    <| List.filter (\x -> case x of
        Just y -> True
        Nothing -> False)
    <| [
        -- left
        grab -1 -1, 
        grab -1 0, 
        grab -1 1, 

        -- middle, exclude center
        grab 0 -1, 
        grab 0 1, 

        -- right
        grab 1 -1, 
        grab 1 0,
        grab 1 1
      ]

{-|
Get the neighbours of a point (x, y) in the matrix
If on edge, then no wrapping happens - they are excluded
-}
indexedNeighbours : Int -> Int -> Matrix a  -> List ((Int, Int), a)
indexedNeighbours x y matrix = 
  let 
    grab di dj =
      let 
        nx = di + x
        ny = dj + y
      in 
        ((nx, ny), Matrix.get nx ny matrix)
  in
    unpackMaybeList (\(pos, x) -> 
      case x of
        Just v -> Just (pos, v)
        Nothing -> Nothing
    )
    <| List.filter (\(_, x) -> case x of
        Just y -> True
        Nothing -> False)
    <| [
        -- left
        grab -1 -1, 
        grab -1 0, 
        grab -1 1, 

        -- middle, exclude center
        grab 0 -1, 
        grab 0 1, 

        -- right
        grab 1 -1, 
        grab 1 0,
        grab 1 1
      ]

{-|
Get the diagonal-neighbours of a point (x, y) in the matrix
If on edge, then no wrapping happens - they are excluded
-}
diagonals : Int -> Int -> Matrix a  -> List a
diagonals x y matrix = 
  let 
    grab di dj =
      Matrix.get (x + di) (y + dj) matrix 
  in
    unpackMaybeList identity
    <| List.filter (\x -> case x of
        Just y -> True
        Nothing -> False)
    <| [
        -- left side
        grab -1 -1, 
        grab -1 1, 
        -- right side
        grab 1 -1, 
        grab 1 1
      ]

{-|
Get the non-diagonal neighbours of a point (x, y) in the matrix
If on edge, then no wrapping happens - they are excluded
-}
neighboursFour : Int -> Int -> Matrix a  -> List a
neighboursFour x y matrix = 
  let 
    grab di dj =
      Matrix.get (x + di) (y + dj) matrix 
  in
    unpackMaybeList identity
    <| List.filter (\x -> case x of
        Just y -> True
        Nothing -> False)
    <| [
        -- left
        grab -1 0, 

        -- middle, exclude center
        grab 0 -1, 
        grab 0 1, 

        -- right
        grab 1 0
      ]

{-|
add two matricies together element by element and return the result
-}
add : Matrix number -> Matrix number -> Maybe (Matrix number)
add a b = map2 (+) a b

{-|
subtract two matricies together element by element and return the result
-}
subtract : Matrix number -> Matrix number -> Maybe (Matrix number)
subtract a b = map2 (-) a b

{-|
take the product of every corresponding element in two matricies and return the result
-}
hadamard : Matrix number -> Matrix number -> Maybe (Matrix number)
hadamard a b = map2 (*) a b

{-|
element-wise power of elements
-}
power : Matrix number -> Matrix number -> Maybe (Matrix number)
power a b = map2 (^) a b

{-|
element wise multiplication 
-}
(.*) : Matrix number -> Matrix number -> Maybe (Matrix number)
(.*) = hadamard

{-|
element wise power 
-}
(.^) : Matrix number -> Matrix number -> Maybe (Matrix number)
(.^) = power
