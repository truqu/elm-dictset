module Tests exposing (..)

import DictSet exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Test exposing (..)


all : Test
all =
    describe "DictSet test suite"
        [ build
        , query
        , combine
        , transform
        ]


build : Test
build =
    describe "Build"
        [ fuzz int
            "Singleton is equal to insert in empty"
          <|
            \x ->
                Expect.equal
                    (insert x (empty identity))
                    (singleton identity x)
        , fuzz int
            "Remove after insert on empty dict is empty dict"
          <|
            \x ->
                Expect.equal
                    (remove x (empty identity))
                    (empty identity)
        ]


query : Test
query =
    describe "Query"
        [ fuzz int
            "An element is a member after insertion"
          <|
            \x ->
                Expect.true "Expected the number to be in the set"
                    (member x <| singleton identity x)
        ]


combine : Test
combine =
    let
        unionOfLists l1 l2 =
            toList <| union (fromList identity l1) (fromList identity l2)

        fromAppendedLists l1 l2 =
            toList <| fromList identity (l1 ++ l2)
    in
        describe "Combine"
            [ fuzz2 (list int)
                (list int)
                "Union of two sets is equal to the set of appended lists"
              <|
                \l1 l2 ->
                    Expect.equal
                        (unionOfLists l1 l2)
                        (fromAppendedLists l1 l2)
            , fuzz (list int)
                "Diff of identical sets is empty"
              <|
                \x ->
                    Expect.equal
                        (diff (fromList identity x) (fromList identity x))
                        (empty identity)
            , fuzz (list int)
                "Intersection of identical sets is the set itself"
              <|
                \x ->
                    Expect.equal
                        (intersect (fromList identity x) (fromList identity x))
                        (fromList identity x)
            ]


transform : Test
transform =
    let
        isEven x =
            x % 2 == 0
    in
        describe "Transform"
            [ fuzz (list int)
                "Map of set of list is equal to set of map of list"
              <|
                \x ->
                    Expect.equal
                        (DictSet.map identity ((*) 2) <| fromList identity x)
                        (fromList identity <| List.map ((*) 2) x)
            , fuzz (list int)
                "Filter of set of list is equal to set of filter of list"
              <|
                \x ->
                    Expect.equal
                        (DictSet.filter isEven <| fromList identity x)
                        (fromList identity <| List.filter isEven x)
            ]
