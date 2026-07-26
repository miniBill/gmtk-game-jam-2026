module Food exposing (Food(..), Ingredient(..), Product, Recipe, all, allIngredients, allProducts, ingredientToColor, ingredientToIcon, ingredientToString, isIngredientVegetarian, isVegetarian, productToColors, productToIcon, productToString, toColors, toIcon, toRecipe, toString)

import Color
import Color.Oklch as Oklch exposing (Oklch)
import Theme


type Food
    = Ingredient Ingredient
    | Product Product


type Ingredient
    = Avocado
    | Blueberry
    | Chicken
    | Chocolate
    | Corn
    | Cow
    | Fish
    | Lettuce
    | Nut
    | Pig
    | Potato
    | Rice
    | Shrimps
    | Sugar
    | Tomato
    | Water
    | Wheat


type Product
    = Bacon
    | Bread
    | Burger
    | Butter
    | Caviar
    | Cereal
    | Cheese
    | ChineseFriedRice
    | ChocolateSpread
    | Cookie
    | Croissant
    | DimSum
    | Egg
    | FishAndChips
    | Fries
    | Guacamole
    | HotDog
    | Jam
    | Milk
    | Omlette
    | Paella
    | PancakeStack
    | Pie
    | Pizza
    | Popcorn
    | Pork
    | Poultry
    | Quesadilla
    | Salad
    | Samosa
    | Sandwich
    | Soup
    | Steak
    | Sushi
    | Taco
    | Yoghurt


all : List Food
all =
    List.map Ingredient allIngredients ++ List.map Product allProducts


allIngredients : List Ingredient
allIngredients =
    [ Avocado, Blueberry, Chicken, Chocolate, Corn, Cow, Fish, Lettuce, Nut, Pig, Potato, Rice, Shrimps, Sugar, Tomato, Water, Wheat ]


allProducts : List Product
allProducts =
    [ Bacon, Bread, Burger, Butter, Caviar, Cereal, Cheese, ChineseFriedRice, ChocolateSpread, Cookie, Croissant, DimSum, Egg, FishAndChips, Fries, Guacamole, HotDog, Jam, Milk, Omlette, Paella, PancakeStack, Pie, Pizza, Popcorn, Pork, Poultry, Quesadilla, Salad, Samosa, Sandwich, Soup, Steak, Sushi, Taco, Yoghurt ]


toIcon : Food -> String
toIcon food =
    case food of
        Ingredient ingredient ->
            ingredientToIcon ingredient

        Product product ->
            productToIcon product


ingredientToIcon : Ingredient -> String
ingredientToIcon ingredient =
    case ingredient of
        Avocado ->
            Theme.iconAvocado

        Blueberry ->
            Theme.iconBlueberry

        Chicken ->
            Theme.iconChicken

        Chocolate ->
            Theme.iconChocolate

        Corn ->
            Theme.iconCorn

        Cow ->
            Theme.iconCow

        Fish ->
            Theme.iconFish

        Lettuce ->
            Theme.iconLettuce

        Nut ->
            Theme.iconNut

        Pig ->
            Theme.iconPig

        Potato ->
            Theme.iconPotato

        Rice ->
            Theme.iconRice

        Shrimps ->
            Theme.iconPrawn

        Sugar ->
            Theme.iconSugar

        Tomato ->
            Theme.iconTomato

        Water ->
            Theme.iconWater

        Wheat ->
            Theme.iconWheat


productToIcon : Product -> String
productToIcon product =
    case product of
        Bacon ->
            Theme.iconBacon

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

        ChineseFriedRice ->
            Theme.iconChineseFriedRice

        ChocolateSpread ->
            Theme.iconChocolateSpread

        Cookie ->
            Theme.iconCookie

        Croissant ->
            Theme.iconCroissant

        DimSum ->
            Theme.iconDimSum

        Egg ->
            Theme.iconEggs

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

        Milk ->
            Theme.iconMilk

        Omlette ->
            Theme.iconOmlette

        Paella ->
            Theme.iconPaella

        PancakeStack ->
            Theme.iconPancakeStack

        Pie ->
            Theme.iconPie

        Pizza ->
            Theme.iconPizza

        Popcorn ->
            Theme.iconPopcorn

        Pork ->
            Theme.iconPork

        Poultry ->
            Theme.iconPoultry

        Quesadilla ->
            Theme.iconQuesadilla

        Salad ->
            Theme.iconSalad

        Samosa ->
            Theme.iconSamosa

        Sandwich ->
            Theme.iconSandwich

        Soup ->
            Theme.iconSoup

        Steak ->
            Theme.iconSteak

        Sushi ->
            Theme.iconSushi

        Taco ->
            Theme.iconTaco

        Yoghurt ->
            Theme.iconYoghurt


toString : Food -> String
toString food =
    case food of
        Ingredient ingredient ->
            ingredientToString ingredient

        Product product ->
            productToString product


ingredientToString : Ingredient -> String
ingredientToString ingredient =
    case ingredient of
        Avocado ->
            "Avocado"

        Blueberry ->
            "Blueberry"

        Chicken ->
            "Chicken"

        Chocolate ->
            "Chocolate"

        Corn ->
            "Corn"

        Cow ->
            "Cow"

        Fish ->
            "Fish"

        Lettuce ->
            "Lettuce"

        Nut ->
            "Nut"

        Pig ->
            "Pig"

        Potato ->
            "Potato"

        Rice ->
            "Rice"

        Shrimps ->
            "Shrimps"

        Sugar ->
            "Sugar"

        Tomato ->
            "Tomato"

        Water ->
            "Water"

        Wheat ->
            "Wheat"


productToString : Product -> String
productToString product =
    case product of
        Bacon ->
            "Bacon"

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

        ChineseFriedRice ->
            "Chinese Fried Rice"

        ChocolateSpread ->
            "Chocolate Spread"

        Cookie ->
            "Cookie"

        Croissant ->
            "Croissant"

        DimSum ->
            "Dim Sum"

        Egg ->
            "Egg"

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

        Milk ->
            "Milk"

        Omlette ->
            "Omlette"

        Paella ->
            "Paella"

        PancakeStack ->
            "Pancake Stack"

        Pie ->
            "Pie"

        Pizza ->
            "Pizza"

        Popcorn ->
            "Popcorn"

        Pork ->
            "Pork"

        Poultry ->
            "Poultry"

        Quesadilla ->
            "Quesadilla"

        Salad ->
            "Salad"

        Samosa ->
            "Samosa"

        Sandwich ->
            "Sandwich"

        Soup ->
            "Soup"

        Steak ->
            "Steak"

        Sushi ->
            "Sushi"

        Taco ->
            "Taco"

        Yoghurt ->
            "Yoghurt"


type alias Recipe =
    List
        { food : Food
        , quantity : Int
        }


toRecipe : Product -> Recipe
toRecipe product =
    case product of
        Bread ->
            [ { food = Ingredient Water, quantity = 1 }
            , { food = Ingredient Wheat, quantity = 1 }
            ]

        Butter ->
            [ { food = Ingredient Cow, quantity = 1 }
            ]

        Cheese ->
            [ { food = Ingredient Cow, quantity = 1 }
            ]

        Egg ->
            [ { food = Ingredient Chicken, quantity = 1 }
            ]

        FishAndChips ->
            [ { food = Ingredient Fish, quantity = 1 }
            , { food = Ingredient Potato, quantity = 1 }
            ]

        Milk ->
            [ { food = Ingredient Cow, quantity = 1 }
            ]

        Fries ->
            [ { food = Ingredient Potato, quantity = 1 }
            ]

        Salad ->
            [ { food = Ingredient Lettuce, quantity = 1 }
            , { food = Ingredient Tomato, quantity = 1 }
            ]

        Steak ->
            [ { food = Ingredient Cow, quantity = 1 }
            ]

        Sushi ->
            [ { food = Ingredient Rice, quantity = 1 }
            , { food = Ingredient Fish, quantity = 1 }
            , { food = Ingredient Avocado, quantity = 1 }
            ]

        Poultry ->
            [ { food = Ingredient Chicken, quantity = 1 }
            ]

        Guacamole ->
            [ { food = Ingredient Avocado, quantity = 1 }
            , { food = Ingredient Corn, quantity = 1 }
            ]

        Caviar ->
            [ { food = Ingredient Fish, quantity = 1 }
            ]

        DimSum ->
            [ { food = Ingredient Shrimps, quantity = 1 }
            , { food = Ingredient Wheat, quantity = 1 }
            ]

        Samosa ->
            [ { food = Ingredient Potato, quantity = 1 }
            , { food = Ingredient Wheat, quantity = 1 }
            ]

        Bacon ->
            [ { food = Ingredient Pig, quantity = 1 }
            ]

        Pork ->
            [ { food = Ingredient Pig, quantity = 1 }
            ]

        Jam ->
            [ { food = Ingredient Blueberry, quantity = 1 }
            , { food = Ingredient Sugar, quantity = 1 }
            ]

        Pizza ->
            [ { food = Product Bread, quantity = 1 }
            , { food = Ingredient Tomato, quantity = 1 }
            , { food = Product Cheese, quantity = 1 }
            ]

        Burger ->
            [ { food = Product Bread, quantity = 1 }
            , { food = Product Bacon, quantity = 1 }
            , { food = Product Cheese, quantity = 1 }
            , { food = Product Steak, quantity = 1 }
            , { food = Ingredient Tomato, quantity = 1 }
            , { food = Ingredient Lettuce, quantity = 1 }
            ]

        Croissant ->
            [ { food = Product Bread, quantity = 1 }
            , { food = Product Butter, quantity = 1 }
            , { food = Ingredient Sugar, quantity = 1 }
            ]

        Popcorn ->
            [ { food = Product Butter, quantity = 1 }
            , { food = Ingredient Corn, quantity = 1 }
            , { food = Ingredient Sugar, quantity = 1 }
            ]

        Taco ->
            [ { food = Product Cheese, quantity = 1 }
            , { food = Ingredient Corn, quantity = 1 }
            , { food = Ingredient Lettuce, quantity = 1 }
            , { food = Product Pork, quantity = 1 }
            , { food = Ingredient Tomato, quantity = 1 }
            ]

        Quesadilla ->
            [ { food = Product Cheese, quantity = 1 }
            , { food = Ingredient Corn, quantity = 1 }
            ]

        PancakeStack ->
            [ { food = Product Butter, quantity = 1 }
            , { food = Ingredient Wheat, quantity = 1 }
            , { food = Product Egg, quantity = 1 }
            , { food = Product Milk, quantity = 1 }
            ]

        Soup ->
            [ { food = Ingredient Water, quantity = 1 }
            , { food = Product Poultry, quantity = 1 }
            ]

        ChineseFriedRice ->
            [ { food = Product Egg, quantity = 1 }
            , { food = Ingredient Rice, quantity = 1 }
            ]

        Paella ->
            [ { food = Ingredient Rice, quantity = 1 }
            , { food = Product Poultry, quantity = 1 }
            , { food = Ingredient Fish, quantity = 1 }
            ]

        Omlette ->
            [ { food = Product Egg, quantity = 1 }
            , { food = Product Bacon, quantity = 1 }
            ]

        HotDog ->
            [ { food = Product Bread, quantity = 1 }
            , { food = Product Pork, quantity = 1 }
            ]

        Sandwich ->
            [ { food = Product Bread, quantity = 1 }
            , { food = Product Jam, quantity = 1 }
            , { food = Ingredient Nut, quantity = 1 }
            ]

        Cookie ->
            [ { food = Ingredient Wheat, quantity = 1 }
            , { food = Product Butter, quantity = 1 }
            , { food = Ingredient Sugar, quantity = 1 }
            , { food = Ingredient Chocolate, quantity = 1 }
            ]

        Yoghurt ->
            [ { food = Product Milk, quantity = 1 }
            , { food = Ingredient Blueberry, quantity = 1 }
            ]

        ChocolateSpread ->
            [ { food = Product Bread, quantity = 1 }
            , { food = Ingredient Chocolate, quantity = 1 }
            , { food = Ingredient Nut, quantity = 1 }
            ]

        Cereal ->
            [ { food = Ingredient Wheat, quantity = 1 }
            , { food = Ingredient Chocolate, quantity = 1 }
            , { food = Product Milk, quantity = 1 }
            ]

        Pie ->
            [ { food = Product Butter, quantity = 1 }
            , { food = Product Milk, quantity = 1 }
            , { food = Ingredient Wheat, quantity = 1 }
            , { food = Ingredient Nut, quantity = 1 }
            ]


toColors : Food -> List Oklch
toColors food =
    case food of
        Ingredient ingredient ->
            [ ingredientToColor ingredient ]

        Product product ->
            productToColors product


productToColors : Product -> List Oklch
productToColors product =
    toRecipe product
        |> List.concatMap (\item -> toColors item.food)
        |> List.map (\oklch -> { oklch | lightness = 0.25 + oklch.lightness * 0.75 })


ingredientToColor : Ingredient -> Oklch
ingredientToColor ingredient =
    case ingredient of
        Avocado ->
            Color.rgb255 0x00 0xFF 0x00 |> Oklch.fromColor

        Blueberry ->
            Color.rgb255 0x99 0x00 0xFF |> Oklch.fromColor

        Chicken ->
            Color.rgb255 0xFF 0x00 0x00 |> Oklch.fromColor

        Chocolate ->
            Color.rgb255 0xBB 0x77 0x00 |> Oklch.fromColor

        Corn ->
            Color.rgb255 0xFF 0xFF 0x00 |> Oklch.fromColor

        Cow ->
            Color.rgb255 0xCC 0xCC 0xCC |> Oklch.fromColor

        Fish ->
            Color.rgb255 0x44 0x44 0xFF |> Oklch.fromColor

        Lettuce ->
            Color.rgb255 0x44 0xFF 0x44 |> Oklch.fromColor

        Nut ->
            Color.rgb255 0x00 0x66 0x00 |> Oklch.fromColor

        Pig ->
            Color.rgb255 0xFF 0x00 0x99 |> Oklch.fromColor

        Potato ->
            Color.rgb255 0xCC 0xFF 0x00 |> Oklch.fromColor

        Rice ->
            Color.rgb255 0xFF 0xFF 0xFF |> Oklch.fromColor

        Shrimps ->
            Color.rgb255 0xFF 0x00 0xDD |> Oklch.fromColor

        Sugar ->
            Color.rgb255 0x00 0xFF 0xFF |> Oklch.fromColor

        Tomato ->
            Color.rgb255 0xFF 0x44 0x00 |> Oklch.fromColor

        Water ->
            Color.rgb255 0x44 0x44 0xFF |> Oklch.fromColor

        Wheat ->
            Color.rgb255 0xFF 0xFF 0x44 |> Oklch.fromColor


isVegetarian : Food -> Bool
isVegetarian food =
    case food of
        Ingredient ingredient ->
            isIngredientVegetarian ingredient

        Product Egg ->
            True

        Product Milk ->
            True

        Product product ->
            toRecipe product
                |> List.all (\line -> isVegetarian line.food)


isIngredientVegetarian : Ingredient -> Bool
isIngredientVegetarian ingredient =
    case ingredient of
        Avocado ->
            True

        Blueberry ->
            True

        Chicken ->
            False

        Chocolate ->
            True

        Corn ->
            True

        Cow ->
            False

        Fish ->
            False

        Lettuce ->
            True

        Nut ->
            True

        Pig ->
            False

        Potato ->
            True

        Rice ->
            True

        Shrimps ->
            False

        Sugar ->
            True

        Tomato ->
            True

        Water ->
            True

        Wheat ->
            True
