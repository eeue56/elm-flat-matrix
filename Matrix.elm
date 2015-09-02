module Matrix where

import Array exposing (Array)
import List

type alias Matrix a = {
  size: (Int, Int),
  data : Array (a) }

repeat : Int -> Int -> a -> Matrix a 
repeat x y v = {
  size = (x, y),
  data = Array.repeat (x * y) v }
  
fromList : List (List a) -> Maybe (Matrix a)
fromList list =
  let
    size' = List.length list
    nestedSize = List.length <| case List.head list of Just x -> x
    allSame = List.isEmpty <| List.filter (\x -> List.length x /= nestedSize) list
  in 
    if not allSame then Nothing
    else Just { size = (size', nestedSize), data = Array.fromList <| List.concat list }
    

get : Int -> Int -> Matrix a  -> Maybe a
get i j matrix = 
  let
    pos = (i * fst matrix.size) + j
  in
    Array.get pos matrix.data

set : a -> Int -> Int -> Matrix a -> Matrix a
set v i j matrix = 
  let
    pos = (i * fst matrix.size) + j
  in
    { matrix | data <- Array.set pos v matrix.data }
