module Matrix.Extra (
    add, subtract, 
    hadamard, (.*),
    power, (.^),
    neighbours) where
{-| Extra methods for Matricies

# Element-wise computation
@docs add, subtract, hadamard, power

# Syntax aliases
@docs (.*), (.^) 

# Interacting with other cells
@docs neighbours

-}
import Matrix exposing (Matrix, map2, get)
import Array exposing (fromList, toList)

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
    List.map (\x -> case x of Just v -> v)
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
