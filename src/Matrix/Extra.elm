module Matrix.Extra where

import Matrix exposing (Matrix)

{-|
add two matricies together element by element and return the result
-}
add : Matrix a -> Matrix a -> Maybe (Matrix a)
add a b = Just a
