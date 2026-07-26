module Food.Dict exposing (FoodDict, all, empty, get, insert, isEmpty, merge, mergeSum, singleton, sum, toList, update)

import FastDict as Dict exposing (Dict)
import Food exposing (Food)


type FoodDict v
    = FoodDict (Dict String ( Food, v ))


empty : FoodDict v
empty =
    FoodDict Dict.empty


singleton : Food -> v -> FoodDict v
singleton k v =
    FoodDict (Dict.singleton (Food.toString k) ( k, v ))


get : Food -> FoodDict v -> Maybe v
get key (FoodDict dict) =
    Dict.get (Food.toString key) dict
        |> Maybe.map Tuple.second


insert : Food -> v -> FoodDict v -> FoodDict v
insert product v (FoodDict dict) =
    FoodDict (Dict.insert (Food.toString product) ( product, v ) dict)


remove : Food -> FoodDict v -> FoodDict v
remove product (FoodDict dict) =
    FoodDict (Dict.remove (Food.toString product) dict)


all : (Food -> v -> Bool) -> FoodDict v -> Bool
all f (FoodDict dict) =
    Dict.foldl (\_ ( p, v ) acc -> acc && f p v) True dict


isEmpty : FoodDict v -> Bool
isEmpty (FoodDict dict) =
    Dict.isEmpty dict


toList : FoodDict v -> List ( Food, v )
toList (FoodDict dict) =
    Dict.values dict


mergeSum : FoodDict Int -> FoodDict Int -> FoodDict Int
mergeSum (FoodDict l) (FoodDict r) =
    FoodDict
        (Dict.merge
            Dict.insert
            (\k ( p, lv ) ( _, rv ) a -> Dict.insert k ( p, lv + rv ) a)
            Dict.insert
            l
            r
            Dict.empty
        )


merge :
    (Food -> l -> out -> out)
    -> (Food -> l -> r -> out -> out)
    -> (Food -> r -> out -> out)
    -> FoodDict l
    -> FoodDict r
    -> out
    -> out
merge inLeft inBoth inRight (FoodDict l) (FoodDict r) seed =
    Dict.merge
        (\_ ( p, v ) acc -> inLeft p v acc)
        (\_ ( p, lv ) ( _, rv ) acc -> inBoth p lv rv acc)
        (\_ ( p, v ) acc -> inRight p v acc)
        l
        r
        seed


sum : FoodDict Int -> Int
sum (FoodDict dict) =
    Dict.foldl (\_ ( _, v ) a -> v + a) 0 dict


update : Food -> (Maybe a -> Maybe a) -> FoodDict a -> FoodDict a
update product f dict =
    case get product dict of
        Nothing ->
            case f Nothing of
                Nothing ->
                    dict

                Just new ->
                    insert product new dict

        (Just _) as v ->
            case f v of
                Nothing ->
                    remove product dict

                Just new ->
                    insert product new dict
