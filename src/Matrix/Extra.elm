module Matrix.Extra where

import Matrix exposing (Matrix)

{-|
add two matricies together element by element and return the result
-}
add : Matrix a -> Matrix a -> Maybe (Matrix a)
add a b = Just a


{-|
subtract two matricies together element by element and return the result
-}
subtract : Matrix a -> Matrix a -> Maybe (Matrix a)
subtract a b = Just a

{-|
take the product of every corresponding element in two matricies and return the result
-}
hadamard : Matrix a -> Matrix a -> Maybe (Matrix a)
hadamard a b = Just a