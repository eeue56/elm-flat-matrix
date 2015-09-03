module Matrix (Matrix, 
  repeat, fromList, 
  get, getRow, getColumn, 
  set, update, concatRow, concatColumn,
  map, map2, indexedMap, filter) where
{-| 
A matrix implemention for Elm.
Internally it uses a flat array for speed reasons.

# The matrix type

@docs Matrix

# Creating a matrix

@docs repeat, fromList

# Dealing with individual elements

@docs get, getRow, getColumn, set, update, concatRow, concatColumn

# Applying functions
@docs filter, map, map2, indexedMap
-}

import Array exposing (Array)
import List

{-|
  Matrix a has a given size, and data contained within
-}
type alias Matrix a = {
  size: (Int, Int),
  data : Array (a) }

{-| 
  Create a matrix of a given size `x y` with a default value of `v`
-}
repeat : Int -> Int -> a -> Matrix a 
repeat x y v = {
  size = (x, y),
  data = Array.repeat (x * y) v }  
  
{-|
  Create a matrix from a list of lists.
  If the lists within the list are not consistently sized, return `Nothing`
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
  Get a value from a given `x y` and return `Just v` if it exists
  Otherwise `Nothing`
-}
get : Int -> Int -> Matrix a  -> Maybe a
get i j matrix = 
  let
    pos = (i * fst matrix.size) + j
  in
    Array.get pos matrix.data

{-| Get a row at a given j
-}
getRow : Int -> Matrix a -> Maybe (Array a)
getRow j matrix =
  let 
    start = (j * fst matrix.size)
    end = start + snd matrix.size
  in
    if end > (fst matrix.size * snd matrix.size) then Nothing
    else Just <| Array.slice start end matrix.data

{-| Get a row at a given i
-}
getColumn : Int -> Matrix a -> Maybe (Array a)
getColumn i matrix =
  if i >= snd matrix.size then Nothing
  else 
    Just
      <| Array.map (\x -> case x of Just v -> v)
      <| Array.filter (\x -> case x of 
        Just _ -> True
        Nothing -> False) 
      <| Array.indexedMap 
        (\i' x -> 
          if i' == i || i % i' /= 0 then Nothing else Just x) matrix.data

{-| Append a matrix to another matrix and return the result. Return nothing if the widths don't match -}
concatColumn : Matrix a -> Matrix a -> Maybe (Matrix a) 
concatColumn a b =
  if fst a.size /= fst b.size then Nothing
  else Just <| { a | size <- (fst a.size + fst b.size, snd a.size), data <- Array.append a.data b.data}

{-| Append a matrix to another matrix and return the result. Return nothing if the widths don't match -}
concatRow : Matrix a -> Matrix a -> Maybe (Matrix a) 
concatRow a b =
  if fst a.size /= fst b.size then Nothing
  else Just <| { a | size <- (fst a.size, snd a.size + snd b.size), data <- Array.append a.data b.data}

{-|
  Set a value at a given `i, j` in the matrix and return the new matrix
  If the `i, j` is out of bounds then return the unmodified matrix
-}
set : Int -> Int -> a -> Matrix a -> Matrix a
set i j v matrix = 
  let
    pos = (i * fst matrix.size) + j
  in
    { matrix | data <- Array.set pos v matrix.data }

{-|
  Update an element at `x, y` with the given update function
  If out of bounds, return the matrix unchanged
-}
update : Int -> Int -> (a -> a) -> Matrix a -> Matrix a
update x y f matrix =
  case get x y matrix of
    Nothing -> matrix
    Just v -> set x y (f v) matrix

{-| 
  Apply a function of every element in the matrix
-}
map : (a -> b) -> Matrix a -> Matrix b
map f matrix = 
  { matrix | data <- Array.map f matrix.data }


{-| Apply a function to two matricies at once
-}
map2 : (a -> b -> c) -> Matrix a -> Matrix b -> Maybe (Matrix c)
map2 f a b = if a.size == b.size 
                then Just { a | data <- Array.fromList <| List.map2 f (Array.toList a.data) (Array.toList b.data) }
                else Nothing

{-| 
  Apply a function, taking the `x, y` of every element in the matrix
-}
indexedMap : (Int -> Int -> a -> b) -> Matrix a -> Matrix b
indexedMap f matrix = 
  let
    f' i v =
      let
        x = i % fst matrix.size
        y = i // fst matrix.size
      in 
        f x y v
  in
    { matrix | data <- Array.indexedMap f' matrix.data }

{-| 
  Keep only elements that return `True` when passed to the given function f
-}
filter : (a -> Bool) -> Matrix a -> Array a
filter f matrix = 
  Array.filter f matrix.data
