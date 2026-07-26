module Product exposing (Product(..), Recipe, all, primary, toColors, toIcon, toRecipe, toString)

import Color
import Color.Oklch as Oklch exposing (Oklch)
import Maybe.Extra
import Theme


type Product
    = Avocado
    | Bacon
    | Blueberry
    | Bread
    | Burger
    | Butter
    | Caviar
    | Cereal
    | Cheese
    | Chicken
    | ChineseFriedRice
    | Chocolate
    | ChocolateSpread
    | Cookie
    | Corn
    | Cow
    | Croissant
    | DimSum
    | Egg
    | Fish
    | FishAndChips
    | Fries
    | Guacamole
    | HotDog
    | Jam
    | Lettuce
    | Milk
    | Nut
    | Omlette
    | Paella
    | PancakeStack
    | Pie
    | Pig
    | Pizza
    | Popcorn
    | Pork
    | Potato
    | Poultry
    | Quesadilla
    | Rice
    | Salad
    | Samosa
    | Sandwich
    | Shrimps
    | Soup
    | Steak
    | Sugar
    | Sushi
    | Taco
    | Tomato
    | Water
    | Wheat
    | Yoghurt


all : List Product
all =
    [ Avocado, Bacon, Blueberry, Bread, Burger, Butter, Caviar, Cereal, Cheese, Chicken, ChineseFriedRice, Chocolate, ChocolateSpread, Cookie, Corn, Cow, Croissant, DimSum, Egg, Fish, FishAndChips, Fries, Guacamole, HotDog, Jam, Lettuce, Milk, Nut, Omlette, Paella, PancakeStack, Pie, Pig, Pizza, Popcorn, Pork, Potato, Poultry, Quesadilla, Rice, Salad, Samosa, Sandwich, Shrimps, Soup, Steak, Sugar, Sushi, Taco, Tomato, Water, Wheat, Yoghurt ]


primary : List Product
primary =
    List.filter (\product -> Maybe.Extra.isNothing (toRecipe product)) all


toIcon : Product -> String
toIcon product =
    case product of
        Avocado ->
            Theme.iconAvocado

        Bacon ->
            Theme.iconBacon

        Blueberry ->
            Theme.iconBlueberry

        Bread ->
            Theme.iconBread

        Burger ->
            Theme.iconHamburger

        Butter ->
            Theme.iconButter

        Caviar ->
            Theme.iconCaviar

        Cereal ->
            Theme.iconCereal

        Cheese ->
            Theme.iconCheese

        Chicken ->
            Theme.iconChicken

        ChineseFriedRice ->
            Theme.iconChineseFriedRice

        Chocolate ->
            Theme.iconChocolate

        ChocolateSpread ->
            Theme.iconChocolateSpread

        Cookie ->
            Theme.iconCookie

        Corn ->
            Theme.iconCorn

        Cow ->
            Theme.iconCow

        Croissant ->
            Theme.iconCroissant

        DimSum ->
            Theme.iconDimSum

        Egg ->
            Theme.iconEggs

        Fish ->
            Theme.iconFish

        FishAndChips ->
            Theme.iconFishAndChips

        Fries ->
            Theme.iconFries

        Guacamole ->
            Theme.iconGuacamole

        HotDog ->
            Theme.iconHotDOg

        Jam ->
            Theme.iconJam

        Lettuce ->
            Theme.iconLettuce

        Milk ->
            Theme.iconMilk

        Nut ->
            Theme.iconNut

        Omlette ->
            Theme.iconOmlette

        Paella ->
            Theme.iconPaella

        PancakeStack ->
            Theme.iconPancakeStack

        Pie ->
            Theme.iconPie

        Pig ->
            Theme.iconPig

        Pizza ->
            Theme.iconPizza

        Popcorn ->
            Theme.iconPopcorn

        Pork ->
            Theme.iconPork

        Potato ->
            Theme.iconPotato

        Poultry ->
            Theme.iconPoultry

        Quesadilla ->
            Theme.iconQuesadilla

        Rice ->
            Theme.iconRice

        Salad ->
            Theme.iconSalad

        Samosa ->
            Theme.iconSamosa

        Sandwich ->
            Theme.iconSandwich

        Shrimps ->
            Theme.iconPrawn

        Soup ->
            Theme.iconSoup

        Steak ->
            Theme.iconSteak

        Sugar ->
            Theme.iconSugar

        Sushi ->
            Theme.iconSushi

        Taco ->
            Theme.iconTaco

        Tomato ->
            Theme.iconTomato

        Water ->
            Theme.iconWater

        Wheat ->
            Theme.iconWheat

        Yoghurt ->
            Theme.iconYoghurt


toString : Product -> String
toString product =
    case product of
        Avocado ->
            "Avocado"

        Bacon ->
            "Bacon"

        Blueberry ->
            "Blueberry"

        Bread ->
            "Bread"

        Burger ->
            "Burger"

        Butter ->
            "Butter"

        Caviar ->
            "Caviar"

        Cereal ->
            "Cereal"

        Cheese ->
            "Cheese"

        Chicken ->
            "Chicken"

        ChineseFriedRice ->
            "Chinese Fried Rice"

        Chocolate ->
            "Chocolate"

        ChocolateSpread ->
            "Chocolate Spread"

        Cookie ->
            "Cookie"

        Corn ->
            "Corn"

        Cow ->
            "Cow"

        Croissant ->
            "Croissant"

        DimSum ->
            "Dim Sum"

        Egg ->
            "Egg"

        Fish ->
            "Fish"

        FishAndChips ->
            "Fish & Chips"

        Fries ->
            "Fries"

        Guacamole ->
            "Guacamole"

        HotDog ->
            "Hot Dog"

        Jam ->
            "Jam"

        Lettuce ->
            "Lettuce"

        Milk ->
            "Milk"

        Nut ->
            "Nut"

        Omlette ->
            "Omlette"

        Paella ->
            "Paella"

        PancakeStack ->
            "Pancake Stack"

        Pie ->
            "Pie"

        Pig ->
            "Pig"

        Pizza ->
            "Pizza"

        Popcorn ->
            "Popcorn"

        Pork ->
            "Pork"

        Potato ->
            "Potato"

        Poultry ->
            "Poultry"

        Quesadilla ->
            "Quesadilla"

        Rice ->
            "Rice"

        Salad ->
            "Salad"

        Samosa ->
            "Samosa"

        Sandwich ->
            "Sandwich"

        Shrimps ->
            "Shrimps"

        Soup ->
            "Soup"

        Steak ->
            "Steak"

        Sugar ->
            "Sugar"

        Sushi ->
            "Sushi"

        Taco ->
            "Taco"

        Tomato ->
            "Tomato"

        Water ->
            "Water"

        Wheat ->
            "Wheat"

        Yoghurt ->
            "Yoghurt"


type alias Recipe =
    List
        { product : Product
        , quantity : Int
        }


toRecipe : Product -> Maybe Recipe
toRecipe product =
    case product of
        Bread ->
            [ { product = Water, quantity = 1 }
            , { product = Wheat, quantity = 1 }
            ]
                |> Just

        Butter ->
            [ { product = Cow, quantity = 1 }
            ]
                |> Just

        Cheese ->
            [ { product = Cow, quantity = 1 }
            ]
                |> Just

        Egg ->
            [ { product = Chicken, quantity = 1 }
            ]
                |> Just

        FishAndChips ->
            [ { product = Fish, quantity = 1 }
            , { product = Potato, quantity = 1 }
            ]
                |> Just

        Milk ->
            [ { product = Cow, quantity = 1 }
            ]
                |> Just

        Fries ->
            [ { product = Potato, quantity = 1 }
            ]
                |> Just

        Salad ->
            [ { product = Lettuce, quantity = 1 }
            , { product = Tomato, quantity = 1 }
            ]
                |> Just

        Steak ->
            [ { product = Cow, quantity = 1 }
            ]
                |> Just

        Sushi ->
            [ { product = Rice, quantity = 1 }
            , { product = Fish, quantity = 1 }
            , { product = Avocado, quantity = 1 }
            ]
                |> Just

        Poultry ->
            [ { product = Chicken, quantity = 1 }
            ]
                |> Just

        Guacamole ->
            [ { product = Avocado, quantity = 1 }
            , { product = Corn, quantity = 1 }
            ]
                |> Just

        Caviar ->
            [ { product = Fish, quantity = 1 }
            ]
                |> Just

        DimSum ->
            [ { product = Shrimps, quantity = 1 }
            , { product = Wheat, quantity = 1 }
            ]
                |> Just

        Samosa ->
            [ { product = Potato, quantity = 1 }
            , { product = Wheat, quantity = 1 }
            ]
                |> Just

        Bacon ->
            [ { product = Pig, quantity = 1 }
            ]
                |> Just

        Pork ->
            [ { product = Pig, quantity = 1 }
            ]
                |> Just

        Jam ->
            [ { product = Blueberry, quantity = 1 }
            , { product = Sugar, quantity = 1 }
            ]
                |> Just

        Pizza ->
            [ { product = Bread, quantity = 1 }
            , { product = Tomato, quantity = 1 }
            , { product = Cheese, quantity = 1 }
            ]
                |> Just

        Burger ->
            [ { product = Bread, quantity = 1 }
            , { product = Bacon, quantity = 1 }
            , { product = Cheese, quantity = 1 }
            , { product = Steak, quantity = 1 }
            , { product = Tomato, quantity = 1 }
            , { product = Lettuce, quantity = 1 }
            ]
                |> Just

        Croissant ->
            [ { product = Bread, quantity = 1 }
            , { product = Butter, quantity = 1 }
            , { product = Sugar, quantity = 1 }
            ]
                |> Just

        Popcorn ->
            [ { product = Butter, quantity = 1 }
            , { product = Corn, quantity = 1 }
            , { product = Sugar, quantity = 1 }
            ]
                |> Just

        Taco ->
            [ { product = Cheese, quantity = 1 }
            , { product = Corn, quantity = 1 }
            , { product = Lettuce, quantity = 1 }
            , { product = Pork, quantity = 1 }
            , { product = Tomato, quantity = 1 }
            ]
                |> Just

        Quesadilla ->
            [ { product = Cheese, quantity = 1 }
            , { product = Corn, quantity = 1 }
            ]
                |> Just

        PancakeStack ->
            [ { product = Butter, quantity = 1 }
            , { product = Wheat, quantity = 1 }
            , { product = Egg, quantity = 1 }
            , { product = Milk, quantity = 1 }
            ]
                |> Just

        Soup ->
            [ { product = Water, quantity = 1 }
            , { product = Poultry, quantity = 1 }
            ]
                |> Just

        ChineseFriedRice ->
            [ { product = Egg, quantity = 1 }
            , { product = Rice, quantity = 1 }
            ]
                |> Just

        Paella ->
            [ { product = Rice, quantity = 1 }
            , { product = Poultry, quantity = 1 }
            , { product = Fish, quantity = 1 }
            ]
                |> Just

        Omlette ->
            [ { product = Egg, quantity = 1 }
            , { product = Bacon, quantity = 1 }
            ]
                |> Just

        HotDog ->
            [ { product = Bread, quantity = 1 }
            , { product = Pork, quantity = 1 }
            ]
                |> Just

        Sandwich ->
            [ { product = Bread, quantity = 1 }
            , { product = Jam, quantity = 1 }
            , { product = Nut, quantity = 1 }
            ]
                |> Just

        Cookie ->
            [ { product = Wheat, quantity = 1 }
            , { product = Butter, quantity = 1 }
            , { product = Sugar, quantity = 1 }
            , { product = Chocolate, quantity = 1 }
            ]
                |> Just

        Yoghurt ->
            [ { product = Milk, quantity = 1 }
            , { product = Blueberry, quantity = 1 }
            ]
                |> Just

        ChocolateSpread ->
            [ { product = Bread, quantity = 1 }
            , { product = Chocolate, quantity = 1 }
            , { product = Nut, quantity = 1 }
            ]
                |> Just

        Cereal ->
            [ { product = Wheat, quantity = 1 }
            , { product = Chocolate, quantity = 1 }
            , { product = Milk, quantity = 1 }
            ]
                |> Just

        Pie ->
            [ { product = Butter, quantity = 1 }
            , { product = Milk, quantity = 1 }
            , { product = Wheat, quantity = 1 }
            , { product = Nut, quantity = 1 }
            ]
                |> Just

        _ ->
            Nothing


toColors : Product -> List Oklch
toColors product =
    case product of
        Avocado ->
            [ Color.rgb255 0x00 0xFF 0x00 |> Oklch.fromColor ]

        Blueberry ->
            [ Color.rgb255 0x99 0x00 0xFF |> Oklch.fromColor ]

        Chicken ->
            [ Color.rgb255 0xFF 0x00 0x00 |> Oklch.fromColor ]

        Chocolate ->
            [ Color.rgb255 0xBB 0x77 0x00 |> Oklch.fromColor ]

        Corn ->
            [ Color.rgb255 0xFF 0xFF 0x00 |> Oklch.fromColor ]

        Cow ->
            [ Color.rgb255 0xCC 0xCC 0xCC |> Oklch.fromColor ]

        Fish ->
            [ Color.rgb255 0x44 0x44 0xFF |> Oklch.fromColor ]

        Lettuce ->
            [ Color.rgb255 0x44 0xFF 0x44 |> Oklch.fromColor ]

        Nut ->
            [ Color.rgb255 0x00 0x66 0x00 |> Oklch.fromColor ]

        Pig ->
            [ Color.rgb255 0xFF 0x00 0x99 |> Oklch.fromColor ]

        Potato ->
            [ Color.rgb255 0xCC 0xFF 0x00 |> Oklch.fromColor ]

        Rice ->
            [ Color.rgb255 0xFF 0xFF 0xFF |> Oklch.fromColor ]

        Shrimps ->
            [ Color.rgb255 0xFF 0x00 0xDD |> Oklch.fromColor ]

        Sugar ->
            [ Color.rgb255 0x00 0xFF 0xFF |> Oklch.fromColor ]

        Tomato ->
            [ Color.rgb255 0xFF 0x44 0x00 |> Oklch.fromColor ]

        Water ->
            [ Color.rgb255 0x44 0x44 0xFF |> Oklch.fromColor ]

        Wheat ->
            [ Color.rgb255 0xFF 0xFF 0x44 |> Oklch.fromColor ]

        _ ->
            toRecipe product
                |> Maybe.withDefault []
                |> List.concatMap (\item -> toColors item.product)
                |> List.map (\oklch -> { oklch | lightness = 0.25 + oklch.lightness * 0.75 })
