module IdDict exposing (IdDict, empty, fold, get, insert, nextId)

import FastDict as Dict exposing (Dict)
import Id exposing (Id(..))


type IdDict k v
    = IdDict (Dict Int v)


empty : IdDict k v
empty =
    IdDict Dict.empty


insert : v -> IdDict k v -> IdDict k v
insert v (IdDict dict) =
    IdDict (Dict.insert (nextId dict) v dict)


get : Id k -> IdDict k v -> Maybe v
get (Id id) (IdDict dict) =
    Dict.get id dict


fold : (Id k -> v -> r -> r) -> r -> IdDict k v -> r
fold f seed (IdDict dict) =
    Dict.foldl (\k v acc -> f (Id k) v acc) seed dict


nextId : Dict Int v -> Int
nextId dict =
    case Dict.getMaxKey dict of
        Nothing ->
            0

        Just existing ->
            existing + 1
