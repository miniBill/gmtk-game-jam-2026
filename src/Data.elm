module Data exposing (Product(..), productToIcon, productToRecipe, productToString)

import Phosphor


type Product
    = Grain
    | Water
    | Bread
    | Cheese
    | Pepper
    | Pizza
    | Milk


productToIcon :
    Product
    -> Phosphor.IconWeight
    -> Phosphor.IconVariant
productToIcon product =
    case product of
        Grain ->
            Phosphor.grains

        Water ->
            Phosphor.drop

        Bread ->
            Phosphor.bread

        Cheese ->
            Phosphor.cheese

        Pepper ->
            Phosphor.pepper

        Pizza ->
            Phosphor.pizza

        Milk ->
            Phosphor.cow


productToString : Product -> String
productToString product =
    case product of
        Grain ->
            "Grain"

        Water ->
            "Water"

        Bread ->
            "Bread"

        Cheese ->
            "Cheese"

        Pepper ->
            "Pepper"

        Pizza ->
            "Pizza"

        Milk ->
            "Milk"


productToRecipe : Product -> Maybe (List { product : Product, quantity : number })
productToRecipe product =
    case product of
        Grain ->
            Nothing

        Water ->
            Nothing

        Bread ->
            [ { product = Grain, quantity = 2 }
            , { product = Water, quantity = 1 }
            ]
                |> Just

        Cheese ->
            [ { product = Milk, quantity = 2 }
            ]
                |> Just

        Pepper ->
            Nothing

        Milk ->
            Nothing

        Pizza ->
            [ { product = Bread, quantity = 1 }
            , { product = Cheese, quantity = 1 }
            , { product = Pepper, quantity = 1 }
            ]
                |> Just
