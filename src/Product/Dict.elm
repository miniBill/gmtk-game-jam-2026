module Product.Dict exposing (ProductDict, all, empty, get, insert, isEmpty, toList)

import FastDict as Dict exposing (Dict)
import Product exposing (Product)


type ProductDict v
    = ProductDict (Dict String ( Product, v ))


empty : ProductDict v
empty =
    ProductDict Dict.empty


get : Product -> ProductDict v -> Maybe v
get key (ProductDict dict) =
    Dict.get (Product.toString key) dict
        |> Maybe.map Tuple.second


insert : Product -> v -> ProductDict v -> ProductDict v
insert product v (ProductDict dict) =
    ProductDict (Dict.insert (Product.toString product) ( product, v ) dict)


all : (Product -> v -> Bool) -> ProductDict v -> Bool
all f (ProductDict dict) =
    Dict.foldl (\_ ( p, v ) acc -> acc && f p v) True dict


isEmpty : ProductDict v -> Bool
isEmpty (ProductDict dict) =
    Dict.isEmpty dict


toList : ProductDict v -> List ( Product, v )
toList (ProductDict dict) =
    Dict.values dict
