module Main exposing (..)

import Check exposing (Claim, quickCheck, claim, that, is, for, true, suite)
import Check.Producer exposing (..)
import Check.Test exposing (evidenceToTest)
import DictSet exposing (..)
import ElmTest exposing (..)


main : Program Never
main =
  runSuite <| evidenceToTest <| quickCheck all

all : Claim
all =
  Check.suite
    "All"
    [ build
    , query
    , combine
    , transform
    ]


build : Claim
build =
  Check.suite
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
  Check.suite
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
    Check.suite
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
    Check.suite
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
