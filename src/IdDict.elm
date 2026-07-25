module IdDict exposing (IdDict, any, empty, fold, get, insert, nextId, update, updateIfExists, values)

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


updateIfExists : Id k -> (v -> v) -> IdDict k v -> IdDict k v
updateIfExists (Id id) f (IdDict dict) =
    IdDict (Dict.update id (Maybe.map f) dict)


update : Id k -> (Maybe v -> Maybe v) -> IdDict k v -> IdDict k v
update (Id id) f (IdDict dict) =
    IdDict (Dict.update id f dict)


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


values : IdDict k v -> List v
values (IdDict dict) =
    Dict.values dict


any : (Id k -> v -> Bool) -> IdDict k v -> Bool
any f (IdDict dict) =
    Dict.stoppableFoldl
        (\k v _ ->
            if f (Id k) v then
                Dict.Stop True

            else
                Dict.Continue False
        )
        False
        dict
