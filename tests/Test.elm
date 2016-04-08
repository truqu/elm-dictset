module Main (..) where

import Check exposing (..)
import Check.Producer exposing (..)
import Check.Test exposing (evidenceToTest)
import Console exposing (IO)
import DictSet exposing (..)
import ElmTest exposing (consoleRunner, Test)
import Signal exposing (Signal)
import Task


console : IO ()
console =
  consoleRunner <| evidenceToTest <| quickCheck all


port runner : Signal (Task.Task x ())
port runner =
  Console.run console


all : Claim
all =
  suite
    "All"
    [ build
    , query
    , combine
    , transform
    ]


build : Claim
build =
  suite
    "Build"
    [ claim
        "Singleton is equal to insert in empty"
        `that` (\x -> insert x (empty identity))
        `is` singleton identity
        `for` int
    , claim
        "Remove after insert on empty dict is empty dict"
        `that` (\x -> remove x (insert x (empty identity)))
        `is` (\x -> empty identity)
        `for` int
    ]


query : Claim
query =
  suite
    "Query"
    [ claim
        "An element is a member after insertion"
        `true` (\x -> member x <| singleton identity x)
        `for` int
    ]


combine : Claim
combine =
  let
    unionOfLists ( l1, l2 ) =
      toList <| union (fromList identity l1) (fromList identity l2)

    fromAppendedLists ( l1, l2 ) =
      toList <| fromList identity (l1 ++ l2)
  in
    suite
      "Combine"
      [ claim
          "Union of two sets is equal to the set of appended lists"
          `that` unionOfLists
          `is` fromAppendedLists
          `for` tuple ( list int, list int )
      , claim
          "Diff of identical sets is empty"
          `that` (\x ->
                    diff (fromList identity x) (fromList identity x)
                 )
          `is` (\x -> empty identity)
          `for` list int
      , claim
          "Intersection of identical sets is the set itself"
          `that` (\x ->
                    toList
                      <| intersect (fromList identity x) (fromList identity x)
                 )
          `is` (\x -> toList <| fromList identity x)
          `for` list int
      ]


transform : Claim
transform =
  let
    isEven x =
      x % 2 == 0
  in
    suite
      "Transform"
      [ claim
          "Map of set of list is equal to set of map of list"
          `that` (\x -> toList <| DictSet.map identity ((*) 2) <| fromList identity x)
          `is` (\x -> toList <| fromList identity <| List.map ((*) 2) x)
          `for` list int
      , claim
          "Filter of set of list is equal to set of filter of list"
          `that` (\x -> toList <| DictSet.filter isEven <| fromList identity x)
          `is` (\x -> toList <| fromList identity <| List.filter isEven x)
          `for` list int
      ]
