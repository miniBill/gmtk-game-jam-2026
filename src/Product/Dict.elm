module Product.Dict exposing (ProductDict, all, empty, get, insert)

import FastDict as Dict exposing (Dict)
import Product exposing (Product)


type ProductDict v
    = ProductDict (Dict String v)


empty : ProductDict v
empty =
    ProductDict Dict.empty


get : Product -> ProductDict v -> Maybe v
get key (ProductDict dict) =
    Dict.get (Product.toString key) dict


insert : Product -> v -> ProductDict v -> ProductDict v
insert product v (ProductDict dict) =
    ProductDict (Dict.insert (Product.toString product) v dict)


all : (v -> Bool) -> ProductDict v -> Bool
all f (ProductDict dict) =
    Dict.foldl (\_ v acc -> acc && f v) True dict
