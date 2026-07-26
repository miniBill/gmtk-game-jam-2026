module Food exposing (Food(..), Ingredient(..), Product, Recipe, all, allIngredients, allProducts, ingredientToColor, ingredientToIcon, ingredientToString, isDuneFood, isDuneIngredient, isDuneProduct, isIngredientVegetarian, isVegetarian, productToColors, productToIcon, productToString, toColors, toIcon, toRecipe, toString)

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
    | Sand
    | Shrimps
    | Sugar
    | Tomato
    | Water
    | Wheat
    | Worm


type Product
    = Bacon
    | Bento
    | Bread
    | Burger
    | Butter
    | Caviar
    | Cereal
    | Cheese
    | ChocolateSpread
    | Cookie
    | Croissant
    | DimSum
    | Dolmades
    | Egg
    | FishFilet
    | FishAndChips
    | FriedChicken
    | FriedRice
    | HotDog
    | IceCream
    | Jam
    | Macaron
    | Milk
    | Omlette
    | Paella
    | Pancakes
    | Pie
    | Pizza
    | Popcorn
    | Pork
    | Poultry
    | Quesadilla
    | RoastBeef
    | Salad
    | Samosa
    | Sandwich
    | Spice
    | Soup
    | Steak
    | Sushi
    | Taco
    | TortillaChips
    | Yoghurt


all : Bool -> List Food
all vegetarian =
    List.map Ingredient (allIngredients vegetarian)
        ++ List.map Product (allProducts vegetarian)


allIngredients : Bool -> List Ingredient
allIngredients vegetarian =
    [ Avocado, Blueberry, Chicken, Chocolate, Corn, Cow, Fish, Lettuce, Nut, Pig, Potato, Rice, Sand, Shrimps, Sugar, Tomato, Water, Wheat, Worm ]
        |> List.filter
            (\ingredient -> isIngredientVegetarian ingredient || not vegetarian)


allProducts : Bool -> List Product
allProducts vegetarian =
    [ Bacon, Bento, Bread, Burger, Butter, Caviar, Cereal, Cheese, ChocolateSpread, Cookie, Croissant, DimSum, Dolmades, Egg, FishFilet, FishAndChips, FriedChicken, FriedRice, HotDog, IceCream, Jam, Macaron, Milk, Omlette, Paella, Pancakes, Pie, Pizza, Popcorn, Pork, Poultry, Quesadilla, RoastBeef, Salad, Samosa, Sandwich, Spice, Soup, Steak, Sushi, Taco, TortillaChips, Yoghurt ]
        |> List.filter (\product -> isVegetarian (Product product) || not vegetarian)


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

        Sand ->
            Theme.iconSand

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

        Worm ->
            Theme.iconWorm


productToIcon : Product -> String
productToIcon product =
    case product of
        Bacon ->
            Theme.iconBacon

        Bento ->
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

        ChocolateSpread ->
            Theme.iconChocolateSpread

        Cookie ->
            Theme.iconCookie

        Croissant ->
            Theme.iconCroissant

        DimSum ->
            Theme.iconDimSum

        Dolmades ->
            Theme.iconDolmades

        Egg ->
            Theme.iconEggs

        FishAndChips ->
            Theme.iconFishAndChips

        FishFilet ->
            Theme.iconFishFilet

        FriedChicken ->
            Theme.iconFriedChicken

        FriedRice ->
            Theme.iconFriedRice

        HotDog ->
            Theme.iconHotDog

        IceCream ->
            Theme.iconIceCream

        Jam ->
            Theme.iconJam

        Macaron ->
            Theme.iconMacaron

        Milk ->
            Theme.iconMilk

        Omlette ->
            Theme.iconOmlette

        Paella ->
            Theme.iconPaella

        Pancakes ->
            Theme.iconPancakes

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

        RoastBeef ->
            Theme.iconRoastBeef

        Salad ->
            Theme.iconSalad

        Samosa ->
            Theme.iconSamosa

        Sandwich ->
            Theme.iconSandwich

        Soup ->
            Theme.iconSoup

        Spice ->
            Theme.iconSpice

        Steak ->
            Theme.iconSteak

        Sushi ->
            Theme.iconSushi

        Taco ->
            Theme.iconTaco

        TortillaChips ->
            Theme.iconTortillaChips

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

        Sand ->
            "Sand"

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

        Worm ->
            "Worm"


productToString : Product -> String
productToString product =
    case product of
        Bacon ->
            "Bacon"

        Bento ->
            "Bento"

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

        ChocolateSpread ->
            "Chocolate Spread™"

        Cookie ->
            "Cookie"

        Croissant ->
            "Croissant"

        DimSum ->
            "Dim Sum"

        Dolmades ->
            "Dolmades"

        Egg ->
            "Egg"

        FishAndChips ->
            "Fish & Chips"

        FishFilet ->
            "Fish Filet"

        FriedChicken ->
            "Fried Chicken"

        FriedRice ->
            "Fried Rice"

        HotDog ->
            "Hot Dog"

        IceCream ->
            "Ice Cream"

        Jam ->
            "Jam"

        Macaron ->
            "Macaron"

        Milk ->
            "Milk"

        Omlette ->
            "Omlette"

        Paella ->
            "Paella"

        Pancakes ->
            "Pancakes"

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

        RoastBeef ->
            "Roast Beef"

        Salad ->
            "Salad"

        Samosa ->
            "Samosa"

        Sandwich ->
            "Sandwich"

        Soup ->
            "Soup"

        Spice ->
            "Spice"

        Steak ->
            "Steak"

        Sushi ->
            "Sushi"

        Taco ->
            "Taco"

        TortillaChips ->
            "TortillaChips"

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

        TortillaChips ->
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

        Pancakes ->
            [ { food = Product Butter, quantity = 1 }
            , { food = Ingredient Wheat, quantity = 1 }
            , { food = Product Egg, quantity = 1 }
            , { food = Product Milk, quantity = 1 }
            ]

        Soup ->
            [ { food = Ingredient Water, quantity = 1 }
            , { food = Product Poultry, quantity = 1 }
            ]

        FriedRice ->
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

        Bento ->
            [ { food = Ingredient Rice, quantity = 1 }
            , { food = Ingredient Shrimps, quantity = 1 }
            , { food = Ingredient Avocado, quantity = 1 }
            ]

        Dolmades ->
            [ { food = Ingredient Rice, quantity = 1 }
            , { food = Ingredient Lettuce, quantity = 1 }
            ]

        FriedChicken ->
            [ { food = Ingredient Chicken, quantity = 1 }
            , { food = Ingredient Wheat, quantity = 1 }
            ]

        IceCream ->
            [ { food = Product Milk, quantity = 1 }
            , { food = Ingredient Sugar, quantity = 1 }
            , { food = Ingredient Chocolate, quantity = 1 }
            ]

        Macaron ->
            [ { food = Product Egg, quantity = 1 }
            , { food = Ingredient Sugar, quantity = 1 }
            , { food = Ingredient Nut, quantity = 1 }
            ]

        RoastBeef ->
            [ { food = Product Steak, quantity = 1 }
            , { food = Ingredient Potato, quantity = 1 }
            ]

        Spice ->
            [ { food = Ingredient Sand, quantity = 1 }
            , { food = Ingredient Worm, quantity = 1 }
            ]

        FishFilet ->
            [ { food = Ingredient Fish, quantity = 1 } ]


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
            Color.rgb255 201 207 103 |> Oklch.fromColor

        Blueberry ->
            Color.rgb255 73 58 145 |> Oklch.fromColor

        Chicken ->
            Color.rgb255 243 177 78 |> Oklch.fromColor

        Chocolate ->
            Color.rgb255 91 49 1 |> Oklch.fromColor

        Corn ->
            Color.rgb255 240 224 0 |> Oklch.fromColor

        Cow ->
            Color.rgb255 125 125 104 |> Oklch.fromColor

        Fish ->
            Color.rgb255 250 128 114 |> Oklch.fromColor

        Lettuce ->
            Color.rgb255 128 192 75 |> Oklch.fromColor

        Nut ->
            Color.rgb255 133 97 69 |> Oklch.fromColor

        Pig ->
            Color.rgb255 255 201 236 |> Oklch.fromColor

        Potato ->
            Color.rgb255 176 137 83 |> Oklch.fromColor

        Rice ->
            Color.rgb255 247 246 251 |> Oklch.fromColor

        Sand ->
            Color.rgb255 244 228 160 |> Oklch.fromColor

        Shrimps ->
            Color.rgb255 230 122 119 |> Oklch.fromColor

        Sugar ->
            Color.rgb255 234 236 234 |> Oklch.fromColor

        Tomato ->
            Color.rgb255 255 99 71 |> Oklch.fromColor

        Water ->
            Color.rgb255 212 241 249 |> Oklch.fromColor

        Wheat ->
            Color.rgb255 245 222 179 |> Oklch.fromColor

        Worm ->
            Color.rgb255 209 173 177 |> Oklch.fromColor


isVegetarian : Food -> Bool
isVegetarian food =
    case food of
        Ingredient ingredient ->
            isIngredientVegetarian ingredient

        Product Egg ->
            True

        Product Milk ->
            True

        Product Spice ->
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

        Sand ->
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

        Worm ->
            True


isDuneFood : Food -> Bool
isDuneFood food =
    case food of
        Ingredient ingredient ->
            isDuneIngredient ingredient

        Product product ->
            isDuneProduct product


isDuneIngredient : Ingredient -> Bool
isDuneIngredient ingredient =
    ingredient == Worm || ingredient == Sand


isDuneProduct : Product -> Bool
isDuneProduct product =
    product == Spice
