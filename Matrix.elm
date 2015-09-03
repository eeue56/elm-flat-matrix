module Matrix where

import Array exposing (Array)
import List

{-|
Matrix a has a given size, and data contained within
-}
type alias Matrix a = {
  size: (Int, Int),
  data : Array (a) }

{-| 
Create a matrix of a given size x y with a default value of v
-}
repeat : Int -> Int -> a -> Matrix a 
repeat x y v = {
  size = (x, y),
  data = Array.repeat (x * y) v }
  
{-|
Create a matrix from a list of lists.
If the lists within the list are not consistently sized, return Nothing
Otherwise return a matrix with the size as the size of the outer and nested lists
-}
fromList : List (List a) -> Maybe (Matrix a)
fromList list =
  let
    size' = List.length list
    nestedSize = List.length <| case List.head list of Just x -> x
    allSame = List.isEmpty <| List.filter (\x -> List.length x /= nestedSize) list
  in 
    if not allSame then Nothing
    else Just { size = (size', nestedSize), data = Array.fromList <| List.concat list }

{-|
Get a value from a given x y and return Just v if it exists
Otherwise Nothing
-}
get : Int -> Int -> Matrix a  -> Maybe a
get i j matrix = 
  let
    pos = (i * fst matrix.size) + j
  in
    Array.get pos matrix.data

{-|
Set a value at a given i, j in the matrix and return the new matrix
If the i, j is out of bounds then return the unmodified matrix
-}
set : Int -> Int -> a -> Matrix a -> Matrix a
set i j v matrix = 
  let
    pos = (i * fst matrix.size) + j
  in
    { matrix | data <- Array.set pos v matrix.data }

{-| 
Apply a function of every element in the matrix
-}
map : (a -> b) -> Matrix a -> Matrix b
map f matrix = 
  { matrix | data <- Array.map f matrix.data }
