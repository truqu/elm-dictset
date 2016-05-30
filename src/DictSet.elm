module DictSet exposing (DictSet, empty, singleton, insert, remove, isEmpty, member, size, union, intersect, diff, toList, fromList, map, foldl, foldr, filter, partition)

{-| A set of unique values. The values can be any type, and the comparison is
done using a function `compare : value -> comparable`.

Insert, remove, and query operations all take *O(log n)* time.

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
import DictSet.Internal exposing (DictSet(..), dict)


{-| Represents a set of unique values together with the compare function
-}
type alias DictSet comparable a =
  DictSet.Internal.DictSet comparable a


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
insert value set =
  let
    compare' =
      DictSet.Internal.compare set
  in
    DictSet compare' <| Dict.insert (compare' value) value (dict set)


{-| Remove a value from a set
-}
remove : a -> DictSet comparable a -> DictSet comparable a
remove value set =
  let
    compare' =
      DictSet.Internal.compare set
  in
    DictSet compare' <| Dict.remove (compare' value) (dict set)


{-| Determine if a set is empty
-}
isEmpty : DictSet comparable a -> Bool
isEmpty set =
  Dict.isEmpty (dict set)


{-| Determine if a value is in a set
-}
member : a -> DictSet comparable a -> Bool
member value set =
  let
    compare =
      DictSet.Internal.compare set
  in
    Dict.member (compare value) (dict set)


{-| Determine the number of elements in a set
-}
size : DictSet comparable a -> Int
size dictset =
  Dict.size (dict dictset)


{-| Get the union of two sets. Keep all values.
-}
union : DictSet comparable a -> DictSet comparable a -> DictSet comparable a
union set1 set2 =
  let
    compare' =
      DictSet.Internal.compare set1
  in
    DictSet compare' <| Dict.union (dict set1) (dict set2)


{-| Get the intersection of two sets. Keeps values that appear in both sets.
-}
intersect : DictSet comparable a -> DictSet comparable a -> DictSet comparable a
intersect set1 set2 =
  let
    compare' =
      DictSet.Internal.compare set1
  in
    DictSet compare' <| Dict.intersect (dict set1) (dict set2)


{-| Get the difference between the first set and the second. Keeps values that do not appear in the second set.
-}
diff : DictSet comparable a -> DictSet comparable a -> DictSet comparable a
diff set1 set2 =
  let
    compare' =
      DictSet.Internal.compare set1
  in
    DictSet compare' <| Dict.diff (dict set1) (dict set2)


{-| Convert a set into a list.
-}
toList : DictSet comparable a -> List a
toList =
  dict >> Dict.values


{-| Convert a list into a set, removing any duplicates.
-}
fromList : (a -> comparable) -> List a -> DictSet comparable a
fromList compare list =
  let
    list' =
      List.map (\v -> ( compare v, v )) list
  in
    DictSet compare <| Dict.fromList list'


{-| Map a function onto a set, creating a new set with no duplicates.
-}
map : (b -> comparable) -> (a -> b) -> DictSet comparable a -> DictSet comparable b
map compare f set =
  fromList compare (List.map f (toList set))


{-| Fold over the values in a set, in order from lowest to highest.
-}
foldl : (a -> b -> b) -> b -> DictSet comparable a -> b
foldl f b set =
  Dict.foldl (\_ v b -> f v b) b (dict set)


{-| Fold over the values in a set, in order from highest to lowest.
-}
foldr : (a -> b -> b) -> b -> DictSet comparable a -> b
foldr f b set =
  Dict.foldr (\_ v b -> f v b) b (dict set)


{-| Create a new set consisting only of elements which satisfy a predicate.
-}
filter : (a -> Bool) -> DictSet comparable a -> DictSet comparable a
filter p set =
  let
    compare =
      DictSet.Internal.compare set
  in
    DictSet compare <| Dict.filter (\_ v -> p v) (dict set)


{-| Create two new sets; the first consisting of elements which satisfy a
-}
partition : (a -> Bool) -> DictSet comparable a -> ( DictSet comparable a, DictSet comparable a )
partition p set =
  let
    compare =
      DictSet.Internal.compare set

    ( d1, d2 ) =
      Dict.partition (\_ v -> p v) (dict set)
  in
    ( DictSet compare d1, DictSet compare d2 )
