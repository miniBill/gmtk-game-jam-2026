module Product.Dict exposing (ProductDict, all, empty, get, insert, isEmpty, merge, mergeSum, singleton, sum, toList, update)

import FastDict as Dict exposing (Dict)
import Product exposing (Product)


type ProductDict v
    = ProductDict (Dict String ( Product, v ))


empty : ProductDict v
empty =
    ProductDict Dict.empty


singleton : Product -> v -> ProductDict v
singleton k v =
    ProductDict (Dict.singleton (Product.toString k) ( k, v ))


get : Product -> ProductDict v -> Maybe v
get key (ProductDict dict) =
    Dict.get (Product.toString key) dict
        |> Maybe.map Tuple.second


insert : Product -> v -> ProductDict v -> ProductDict v
insert product v (ProductDict dict) =
    ProductDict (Dict.insert (Product.toString product) ( product, v ) dict)


remove : Product -> ProductDict v -> ProductDict v
remove product (ProductDict dict) =
    ProductDict (Dict.remove (Product.toString product) dict)


all : (Product -> v -> Bool) -> ProductDict v -> Bool
all f (ProductDict dict) =
    Dict.foldl (\_ ( p, v ) acc -> acc && f p v) True dict


isEmpty : ProductDict v -> Bool
isEmpty (ProductDict dict) =
    Dict.isEmpty dict


toList : ProductDict v -> List ( Product, v )
toList (ProductDict dict) =
    Dict.values dict


mergeSum : ProductDict Int -> ProductDict Int -> ProductDict Int
mergeSum (ProductDict l) (ProductDict r) =
    ProductDict
        (Dict.merge
            Dict.insert
            (\k ( p, lv ) ( _, rv ) a -> Dict.insert k ( p, lv + rv ) a)
            Dict.insert
            l
            r
            Dict.empty
        )


merge :
    (Product -> l -> out -> out)
    -> (Product -> l -> r -> out -> out)
    -> (Product -> r -> out -> out)
    -> ProductDict l
    -> ProductDict r
    -> out
    -> out
merge inLeft inBoth inRight (ProductDict l) (ProductDict r) seed =
    Dict.merge
        (\_ ( p, v ) acc -> inLeft p v acc)
        (\_ ( p, lv ) ( _, rv ) acc -> inBoth p lv rv acc)
        (\_ ( p, v ) acc -> inRight p v acc)
        l
        r
        seed


sum : ProductDict Int -> Int
sum (ProductDict dict) =
    Dict.foldl (\_ ( _, v ) a -> v + a) 0 dict


update : Product -> (Maybe a -> Maybe a) -> ProductDict a -> ProductDict a
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
