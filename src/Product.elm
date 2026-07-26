module Product exposing (Product(..), Recipe, all, primary, toColor, toIcon, toRecipe, toString)

import Color.Oklch as Oklch
import Maybe.Extra
import Theme


type Product
    = Grain
    | Water
    | Bread
    | Milk
    | Cheese
    | Pizza


all : List Product
all =
    [ Grain
    , Water
    , Bread
    , Milk
    , Cheese
    , Pizza
    ]


primary : List Product
primary =
    List.filter (\product -> Maybe.Extra.isNothing (toRecipe product)) all


toIcon : Product -> String
toIcon product =
    case product of
        Grain ->
            Theme.iconGrains

        Water ->
            Theme.iconWater

        Bread ->
            Theme.iconBread

        Cheese ->
            Theme.iconCheese

        Pizza ->
            Theme.iconPizza

        Milk ->
            Theme.iconCow


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

        Pizza ->
            "Pizza"

        Milk ->
            "Milk"


type alias Recipe =
    List
        { product : Product
        , quantity : Int
        }


toRecipe : Product -> Maybe Recipe
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

        Milk ->
            Nothing

        Pizza ->
            [ { product = Bread, quantity = 1 }
            , { product = Cheese, quantity = 1 }
            ]
                |> Just


toColor : Product -> String
toColor product =
    let
        std : Float -> String
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

        Pizza ->
            std 330
