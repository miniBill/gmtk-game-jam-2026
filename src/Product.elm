module Product exposing (Product(..), all, primary, toColor, toIcon, toRecipe, toString)

import Color.Oklch as Oklch
import Phosphor


type Product
    = Grain
    | Water
    | Bread
    | Milk
    | Cheese
    | Pepper
    | Pizza


all : List Product
all =
    [ Grain
    , Water
    , Bread
    , Milk
    , Cheese
    , Pepper
    , Pizza
    ]


primary : List Product
primary =
    List.filter (\product -> toRecipe product == Nothing) all


toIcon :
    Product
    -> Phosphor.IconWeight
    -> Phosphor.IconVariant
toIcon product =
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


toString : Product -> String
toString product =
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


toRecipe : Product -> Maybe (List { product : Product, quantity : number })
toRecipe product =
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


toColor : Product -> String
toColor product =
    let
        std hue =
            Oklch.toCssString
                { lightness = 0.75
                , chroma = 0.125
                , hue = hue / 360
                , alpha = 1
                }
    in
    case product of
        Grain ->
            std 105

        Water ->
            std 230

        Bread ->
            std 80

        Milk ->
            "white"

        Cheese ->
            std 60

        Pepper ->
            std 18

        Pizza ->
            std 330
