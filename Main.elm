module Main where

import Text
import Graphics.Element exposing (..)
import ElmTest.Run as Run
import ElmTest.Runner.Element as ElementRunner
import ElmTest.Runner.String  as StringRunner
import ElmTest.Test exposing (..)
import ElmTest.Assertion exposing (..)

import Matrix


fromList : Test
fromList = suite "From list"
  [ test "equal size" 
      <| assertEqual (2, 2) 
      <| case Matrix.fromList [[1, 1], [1, 1]] of Just v -> v.size,
    test "inequal size" 
      <| assertEqual (3, 2) 
      <| case Matrix.fromList [[1, 1], [1, 1], [3, 3]] of Just v -> v.size,
    test "inequal size" 
      <| assertEqual (2, 3) 
      <| case Matrix.fromList [[1, 1, 1], [1, 1, 1]] of Just v -> v.size,
    test "Non-consistent size" 
      <| assertEqual False 
      <| case Matrix.fromList [[1, 1, 1], [1, 1, 1, 5]] of 
        Just v -> True
        Nothing -> False
  ]

get : Test 
get = suite "Get"
  [ test "get first" 
      <| assertEqual (Just 1) 
      <| Matrix.get 0 0 <| Matrix.repeat 1 1 1,
    test "get invalid range" 
      <| assertEqual (Nothing) 
      <| Matrix.get 1 2 <| Matrix.repeat 1 1 1
  ]

set : Test
set = suite "Set"
  [ test "Set first" 
      <| assertEqual (Just 5) 
      <| Matrix.get 0 0 <| Matrix.set 0 0 5 <| Matrix.repeat 2 2 1,
    test "Set outside of range does nothing"
      <| (\x -> assertEqual (1, 1) x.size)
      <| Matrix.set 5 5 1 <| Matrix.repeat 1 1 1
  ]

map : Test
map = suite "Map"
  [ test "increment every value" 
      <| assertEqual (Matrix.repeat 2 2 2) 
      <|  Matrix.map (\x -> x + 1) <| Matrix.repeat 2 2 1,
    test "identity" 
      <| assertEqual (Matrix.repeat 2 2 1) 
      <|  Matrix.map identity <| Matrix.repeat 2 2 1
  ]

tests : Test
tests = suite "Tests"
  [ get,
    set,
    fromList, 
    map
  ]

results : String
results = StringRunner.runDisplay tests

main : Element
main = ElementRunner.runDisplay tests