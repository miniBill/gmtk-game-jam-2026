module IdDict exposing (IdDict, any, empty, fold, get, insert, insertNew, update, updateIfExists, updateWith)

import FastDict as Dict exposing (Dict)
import Id exposing (Id(..))


type IdDict k v
    = IdDict (Dict Int v)


empty : IdDict k v
empty =
    IdDict Dict.empty


insertNew : v -> IdDict k v -> IdDict k v
insertNew v (IdDict dict) =
    IdDict (Dict.insert (nextId dict) v dict)


insert : Id k -> v -> IdDict k v -> IdDict k v
insert (Id k) v (IdDict dict) =
    IdDict (Dict.insert k v dict)


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


merge :
    (Id k -> l -> o -> o)
    -> (Id k -> l -> r -> o -> o)
    -> (Id k -> r -> o -> o)
    -> IdDict k l
    -> IdDict k r
    -> o
    -> o
merge left both right (IdDict leftDict) (IdDict rightDict) seed =
    Dict.merge
        (\lk lv lacc -> left (Id lk) lv lacc)
        (\bk lv rv bacc -> both (Id bk) lv rv bacc)
        (\rk rv racc -> right (Id rk) rv racc)
        leftDict
        rightDict
        seed


updateWith :
    IdDict k new
    ->
        { inBoth : Id k -> old -> new -> IdDict k old -> IdDict k old
        , inNew : Id k -> new -> IdDict k old -> IdDict k old
        }
    -> IdDict k old
    -> IdDict k old
updateWith new { inBoth, inNew } old =
    merge
        (\_ _ acc -> acc)
        inBoth
        inNew
        old
        new
        old
