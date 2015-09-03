module Matrix.Extra where

import Matrix exposing (Matrix)
import Array exposing (fromList, toList)
import List exposing (map2)

map2 f a b = if a.size == b.size 
                then Just { a | data <- Array.fromList <| List.map2 f (Array.toList a.data) (Array.toList b.data) }
                else Nothing

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