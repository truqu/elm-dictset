module DictSet exposing
    ( DictSet
    , empty, singleton, insert, remove
    , isEmpty, member, size
    , union, intersect, diff
    , toList, fromList
    , map, foldl, foldr, filter, partition
    )

{-| A set of unique values. The values can be any type, and the comparison is
done using a function `compare : value -> comparable`.

Insert, remove, and query operations all take _O(log n)_ time.


# Sets

@docs DictSet


# Build

@docs empty, singleton, insert, remove


# Query

@docs isEmpty, member, size


# Combine

@docs union, intersect, diff


# Lists

@docs toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition

-}

import Dict exposing (Dict)


{-| Represents a set of unique values together with the compare function
-}
type DictSet comparable a
    = DictSet (a -> comparable) (Dict comparable a)


{-| Create an empty set
-}
empty : (a -> comparable) -> DictSet comparable a
empty compare =
    DictSet compare Dict.empty


{-| Create a set with one value
-}
singleton : (a -> comparable) -> a -> DictSet comparable a
singleton compare value =
    insert value (empty compare)


{-| Insert a new value into a set
-}
insert : a -> DictSet comparable a -> DictSet comparable a
insert value (DictSet compare dict) =
    DictSet compare <| Dict.insert (compare value) value dict


{-| Remove a value from a set
-}
remove : a -> DictSet comparable a -> DictSet comparable a
remove value (DictSet compare dict) =
    DictSet compare <| Dict.remove (compare value) dict


{-| Determine if a set is empty
-}
isEmpty : DictSet comparable a -> Bool
isEmpty (DictSet _ dict) =
    Dict.isEmpty dict


{-| Determine if a value is in a set
-}
member : a -> DictSet comparable a -> Bool
member value (DictSet compare dict) =
    Dict.member (compare value) dict


{-| Determine the number of elements in a set
-}
size : DictSet comparable a -> Int
size (DictSet _ dict) =
    Dict.size dict


{-| Get the union of two sets. Keep all values.
-}
union : DictSet comparable a -> DictSet comparable a -> DictSet comparable a
union (DictSet compare dict1) (DictSet _ dict2) =
    DictSet compare <| Dict.union dict1 dict2


{-| Get the intersection of two sets. Keeps values that appear in both sets.
-}
intersect : DictSet comparable a -> DictSet comparable a -> DictSet comparable a
intersect (DictSet compare dict1) (DictSet _ dict2) =
    DictSet compare <| Dict.intersect dict1 dict2


{-| Get the difference between the first set and the second. Keeps values that do not appear in the second set.
-}
diff : DictSet comparable a -> DictSet comparable a -> DictSet comparable a
diff (DictSet compare dict1) (DictSet _ dict2) =
    DictSet compare <| Dict.diff dict1 dict2


{-| Convert a set into a list.
-}
toList : DictSet comparable a -> List a
toList (DictSet _ dict) =
    Dict.values dict


{-| Convert a list into a set, removing any duplicates.
-}
fromList : (a -> comparable) -> List a -> DictSet comparable a
fromList compare list =
    List.map (\v -> ( compare v, v )) list
        |> Dict.fromList
        |> DictSet compare


{-| Map a function onto a set, creating a new set with no duplicates.
-}
map : (b -> comparable) -> (a -> b) -> DictSet comparable a -> DictSet comparable b
map compare f set =
    fromList compare (List.map f (toList set))


{-| Fold over the values in a set, in order from lowest to highest.
-}
foldl : (a -> b -> b) -> b -> DictSet comparable a -> b
foldl f initialAcc (DictSet _ dict) =
    Dict.foldl (\_ v acc -> f v acc) initialAcc dict


{-| Fold over the values in a set, in order from highest to lowest.
-}
foldr : (a -> b -> b) -> b -> DictSet comparable a -> b
foldr f initialAcc (DictSet _ dict) =
    Dict.foldr (\_ v acc -> f v acc) initialAcc dict


{-| Create a new set consisting only of elements which satisfy a predicate.
-}
filter : (a -> Bool) -> DictSet comparable a -> DictSet comparable a
filter p (DictSet compare dict) =
    DictSet compare <| Dict.filter (\_ v -> p v) dict


{-| Create two new sets; the first consisting of elements which satisfy a
-}
partition : (a -> Bool) -> DictSet comparable a -> ( DictSet comparable a, DictSet comparable a )
partition p (DictSet compare dict) =
    let
        ( d1, d2 ) =
            Dict.partition (\_ v -> p v) dict
    in
    ( DictSet compare d1, DictSet compare d2 )
