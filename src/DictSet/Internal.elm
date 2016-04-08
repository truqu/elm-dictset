module DictSet.Internal (DictSet(..), compare, dict) where

import Dict exposing (Dict)


type DictSet comparable a
  = DictSet (a -> comparable) (Dict comparable a)


compare : DictSet comparable a -> (a -> comparable)
compare set =
  case set of
    DictSet function _ ->
      function


dict : DictSet comparable a -> Dict comparable a
dict set =
  case set of
    DictSet _ dict ->
      dict
