module Data exposing (Product(..), productToIcon, productToString)

import Phosphor


type Product
    = Grain
    | Water
    | Bread
    | Cheese
    | Pepper
    | Pizza


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
