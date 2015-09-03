module Matrix.Extra (add, subtract, hadamard, (.*)) where
{-| Extra methods for Matricies

# Element-wise computation
@docs add, subtract, hadamard, (.*)

-}
import Matrix exposing (Matrix, map2)
import Array exposing (fromList, toList)



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
element wise multiplication 
-}
(.*) : Matrix number -> Matrix number -> Maybe (Matrix number)
(.*) = hadamard