module Matrix (Matrix, 
  height, width,
  repeat, fromList, 
  get, getRow, getColumn, 
  set, update, concatVertical, concatHorizontal,
  toIndexedArray, empty,
  map, map2, indexedMap, filter) where
{-| 
A matrix implemention for Elm.
Internally it uses a flat array for speed reasons.

# The matrix type

@docs Matrix

# Creating a matrix

@docs repeat, fromList, empty

# Get matrix dimensions

@docs height, width

# Dealing with individual elements

@docs get, set, update 

# Appending to an Matrix

@docs concatVertical, concatHorizontal

# Get rows/columns

@docs getRow, getColumn

# Applying functions
@docs filter, map, map2, indexedMap, toIndexedArray
-}

import Array exposing (Array)
import List

{-|
  Matrix a has a given size, and data contained within
-}
type alias Matrix a = {
  size: (Int, Int),
  data : Array (a) }


{-| Create an empty matrix -}
empty : Matrix a
empty = { size = (0, 0), data = Array.empty}

{-| Width of a given matrix -}
width : Matrix a -> Int
width matrix = fst matrix.size 

{-| Height of a given matrix -}
height : Matrix a -> Int
height matrix = snd matrix.size 

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
    -- the number of elements in the top level list is taken as height
    height = List.length list
    -- the number of elements in the first element is taken as the width
    width = List.length <|
      case List.head list of
        Just x -> x
        Nothing -> []
    -- ensure that all "rows" are the same size
    allSame = List.isEmpty <| List.filter (\x -> List.length x /= width) list
  in 
    if not allSame then Nothing
    else Just { size = (width, height), data = Array.fromList <| List.concat list }

{-|
  Get a value from a given `x y` and return `Just v` if it exists
  Otherwise `Nothing`
-}
get : Int -> Int -> Matrix a -> Maybe a
get i j matrix = 
  let
    pos = (j * (width matrix)) + i
  in
    if (i < width matrix && i > -1) && (j < height matrix && j > -1) then Array.get pos matrix.data
    else Nothing

{-| Get a row at a given j
-}
getRow : Int -> Matrix a -> Maybe (Array a)
getRow j matrix =
  let 
    start = (j * (width matrix) )
    end = start + width matrix
  in
    if end > ((width matrix) * (height matrix)) then Nothing
    else Just <| Array.slice start end matrix.data

{-| Get a row at a given i
-}
getColumn : Int -> Matrix a -> Maybe (Array a)
getColumn i matrix =
  let 
    width = fst matrix.size
    height = snd matrix.size
    indices = List.map (\x -> x*width + i) [0..(height-1)]
  in 
    if i >= width 
      then Nothing
      else Just <| Array.fromList <| List.foldl (\index ls ->
        case Array.get index matrix.data of
          Just v -> ls ++ [v]
          Nothing -> ls
      ) [] indices

{-| Append a matrix to another matrix horizontally and return the result. Return Nothing if the heights don't match -}
concatHorizontal : Matrix a -> Matrix a -> Maybe (Matrix a) 
concatHorizontal a b =
  let
    finalWidth = fst a.size + fst b.size
    insert i xs array = Array.append
                          (Array.append (Array.slice 0 i array) xs)
                          (Array.slice i (Array.length array) array)
  in
    if snd a.size /= snd b.size then Nothing
    else Just <| { a | size = (finalWidth, snd a.size)
                     , data = List.foldl 
                                (\(i,xs) acc -> insert (i*finalWidth) xs acc)
                                b.data
                                <| List.foldl (\i ls ->
                                  case getRow i a of
                                    Just v -> ls ++ [(i,v)]
                                    Nothing -> ls) [] [0..(snd a.size)-1]
                 }

{-| Append a matrix to another matrix vertically and return the result. Return Nothing if the widths don't match -}
concatVertical : Matrix a -> Matrix a -> Maybe (Matrix a) 
concatVertical a b =
  if fst a.size /= fst b.size then Nothing
  else Just <| { a | size = (fst a.size, snd a.size + snd b.size), data = Array.append a.data b.data}

{-|
  Set a value at a given `i, j` in the matrix and return the new matrix
  If the `i, j` is out of bounds then return the unmodified matrix
-}
set : Int -> Int -> a -> Matrix a -> Matrix a
set i j v matrix = 
  let
    pos = (j * fst matrix.size) + i
  in
    if i < width matrix && j < height matrix then 
      { matrix | data = Array.set pos v matrix.data }
    else 
      matrix

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
  { matrix | data = Array.map f matrix.data }


{-| Apply a function to two matricies at once
-}
map2 : (a -> b -> c) -> Matrix a -> Matrix b -> Maybe (Matrix c)
map2 f a b = if a.size == b.size 
                then Just { a | data = Array.fromList <| List.map2 f (Array.toList a.data) (Array.toList b.data) }
                else Nothing

{-| 
  Apply a function, taking the `x, y` of every element in the matrix
-}
indexedMap : (Int -> Int -> a -> b) -> Matrix a -> Matrix b
indexedMap f matrix = 
  let
    f' i v =
      let
        x = i % (width matrix) 
        y = i // (width matrix)
      in 
        f x y v
  in
    { matrix | data = Array.fromList <| List.indexedMap f' <| Array.toList matrix.data }

{-| 
  Keep only elements that return `True` when passed to the given function f
-}
filter : (a -> Bool) -> Matrix a -> Array a
filter f matrix = 
  Array.filter f matrix.data

{-| Convert a matrix to an indexed array
-}
toIndexedArray : Matrix a -> Array ((Int, Int), a)
toIndexedArray matrix = (indexedMap (\x y v -> ((x, y), v)) matrix).data
