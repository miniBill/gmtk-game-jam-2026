module Turn exposing (addNewPlanets, randomColonyRequest, run)

import Angle
import Food exposing (Food, Ingredient)
import Food.Dict exposing (FoodDict)
import Id exposing (Id, PlanetId)
import IdDict exposing (IdDict)
import Length exposing (Length, Meters)
import List.Extra
import Point2d exposing (Point2d)
import Quantity
import Random
import String.Extra
import Types exposing (FarmData, GameMode(..), GamePhase(..), LostModel, OccupiedPlanet(..), Planet, PlanetKind(..), PlayingModel)


minimumPlanetDistance : Length
minimumPlanetDistance =
    Length.lightYears 0.75


ringWidth : Length
ringWidth =
    Length.lightYears 1


run : PlayingModel -> Result LostModel PlayingModel
run model =
    let
        ( newModel, hasLost ) =
            model
                |> calculateExports
                |> applyImport
                |> addNewPlanets
                |> updateCountdowns
    in
    if hasLost then
        { initialSeed = newModel.initialSeed
        , vegetarian = newModel.vegetarian
        , planets = newModel.planets
        , selected = newModel.selected
        , highlighted = newModel.highlighted
        , score = newModel.score
        , gameMode = newModel.gameMode
        , turns = newModel.turns
        }
            |> Err

    else
        Ok newModel


calculateExports :
    PlayingModel
    ->
        ( PlayingModel
        , IdDict PlanetId (FoodDict Int)
        )
calculateExports model =
    let
        ( newPlanets, exports ) =
            IdDict.fold
                calculateExport
                ( model.planets, IdDict.empty )
                model.planets
    in
    ( { model | planets = newPlanets }, exports )


calculateExport :
    Id PlanetId
    -> Planet
    -> ( IdDict PlanetId Planet, IdDict PlanetId (FoodDict Int) )
    -> ( IdDict PlanetId Planet, IdDict PlanetId (FoodDict Int) )
calculateExport planetId planet (( accPlanets, accExports ) as acc) =
    let
        calculate :
            FoodDict Int
            ->
                ( IdDict PlanetId (FoodDict Int)
                , FoodDict Int
                )
        calculate available =
            IdDict.fold
                (\to wanted ( exporting, remaining ) ->
                    let
                        ( toAdd, newRemaining ) =
                            Food.Dict.merge
                                (\_ _ iacc -> iacc)
                                (\product want remain ( exportAcc, remainAcc ) ->
                                    let
                                        quantity : Int
                                        quantity =
                                            min want remain
                                    in
                                    ( Food.Dict.insert product quantity exportAcc
                                    , Food.Dict.insert product (remain - quantity) remainAcc
                                    )
                                )
                                (\_ _ iacc -> iacc)
                                wanted
                                remaining
                                ( Food.Dict.empty, remaining )
                    in
                    ( IdDict.insert to toAdd exporting, newRemaining )
                )
                ( IdDict.empty, available )
                planet.links

        merge : IdDict PlanetId (FoodDict Int) -> IdDict PlanetId (FoodDict Int)
        merge calculated =
            IdDict.updateWith calculated
                { inBoth =
                    \k l r iacc ->
                        IdDict.insert k
                            (Food.Dict.mergeSum l r)
                            iacc
                , inNew = \k v iacc -> IdDict.insert k v iacc
                }
                accExports
    in
    case planet.kind of
        VirginPlanet _ ->
            acc

        ColonyPlanet _ ->
            acc

        OccupiedPlanet (FarmPlanet farm) ->
            let
                available : FoodDict Int
                available =
                    if farm.countdown > 0 then
                        Food.Dict.singleton
                            (Food.Ingredient farm.ingredient)
                            farm.perTurn

                    else
                        Food.Dict.empty
            in
            ( accPlanets, merge (Tuple.first (calculate available)) )

        OccupiedPlanet (FactoryPlanet factory) ->
            let
                available : FoodDict Int
                available =
                    Maybe.map
                        (\product ->
                            let
                                quantity : Int
                                quantity =
                                    List.foldl
                                        (\line iacc ->
                                            let
                                                has : Int
                                                has =
                                                    Food.Dict.get line.food factory.deposit
                                                        |> Maybe.withDefault 0
                                            in
                                            min iacc (has // line.quantity)
                                        )
                                        factory.efficiency
                                        (Food.toRecipe product)
                            in
                            Food.Dict.singleton (Food.Product product) quantity
                        )
                        factory.product
                        |> Maybe.withDefault Food.Dict.empty
            in
            ( IdDict.insert planetId
                { planet
                    | kind =
                        OccupiedPlanet
                            (FactoryPlanet
                                { factory | deposit = Food.Dict.empty }
                            )
                }
                accPlanets
            , merge (Tuple.first (calculate available))
            )

        OccupiedPlanet (DepositPlanet deposit) ->
            let
                ( exports, remaining ) =
                    calculate deposit.content
            in
            ( IdDict.insert planetId
                { planet
                    | kind =
                        OccupiedPlanet
                            (DepositPlanet
                                { deposit | content = remaining }
                            )
                }
                accPlanets
            , merge exports
            )


applyImport :
    ( PlayingModel
    , IdDict PlanetId (FoodDict Int)
    )
    -> PlayingModel
applyImport ( model, exports ) =
    { model
        | planets =
            IdDict.updateWith exports
                { inBoth =
                    \planetId planet imports acc ->
                        if Food.Dict.all (\_ q -> q == 0) imports then
                            acc

                        else
                            case planet.kind of
                                VirginPlanet _ ->
                                    acc

                                ColonyPlanet colony ->
                                    case Food.Dict.get colony.product imports of
                                        Just got ->
                                            IdDict.insert planetId
                                                { planet
                                                    | kind =
                                                        ColonyPlanet
                                                            { colony | quantity = max 0 (colony.quantity - got) }
                                                }
                                                acc

                                        Nothing ->
                                            acc

                                OccupiedPlanet (FarmPlanet _) ->
                                    acc

                                OccupiedPlanet (FactoryPlanet factory) ->
                                    IdDict.insert planetId
                                        { planet | kind = OccupiedPlanet (FactoryPlanet { factory | deposit = imports }) }
                                        acc

                                OccupiedPlanet (DepositPlanet deposit) ->
                                    let
                                        newContent : FoodDict Int
                                        newContent =
                                            fillDepositWith imports deposit
                                    in
                                    IdDict.insert planetId
                                        { planet | kind = OccupiedPlanet (DepositPlanet { deposit | content = newContent }) }
                                        acc
                , inNew =
                    \_ _ acc ->
                        -- Shouldn't happen
                        acc
                }
                model.planets
    }


fillDepositWith : FoodDict Int -> Types.DepositData -> FoodDict Int
fillDepositWith imports { capacity, content } =
    case capacity of
        Nothing ->
            Food.Dict.mergeSum content imports

        Just total ->
            let
                availableCapacity : Int
                availableCapacity =
                    total - Food.Dict.sum content
            in
            fillDepositStep availableCapacity (Food.Dict.toList imports) [] content


fillDepositStep : Int -> List ( Food, Int ) -> List ( Food, Int ) -> FoodDict Int -> FoodDict Int
fillDepositStep availableCapacity queue next content =
    if availableCapacity <= 0 then
        content

    else
        case queue of
            [] ->
                case next of
                    [] ->
                        content

                    _ :: _ ->
                        fillDepositStep availableCapacity next [] content

            ( product, quantity ) :: tail ->
                if quantity <= 0 then
                    fillDepositStep availableCapacity tail next content

                else
                    fillDepositStep (availableCapacity - 1)
                        tail
                        (( product, quantity - 1 ) :: next)
                        (Food.Dict.update product
                            (\found ->
                                found
                                    |> Maybe.withDefault 0
                                    |> (+) 1
                                    |> Just
                            )
                            content
                        )


updateCountdowns : PlayingModel -> ( PlayingModel, Bool )
updateCountdowns model =
    IdDict.fold
        (\planetId planet (( modelAcc, lost ) as acc) ->
            case updateCountdown model planet of
                PlanetUnchanged ->
                    acc

                PlanetNotLost newPlanet ->
                    ( { modelAcc
                        | planets =
                            IdDict.insert planetId newPlanet modelAcc.planets
                      }
                    , lost
                    )

                PlanetLost newPlanet ->
                    ( { modelAcc
                        | planets = IdDict.insert planetId newPlanet modelAcc.planets
                      }
                    , True
                    )

                PlanetScored points newPlanetGenerator ->
                    let
                        ( newPlanet, newSeed ) =
                            Random.step newPlanetGenerator modelAcc.currentSeed
                    in
                    ( { modelAcc
                        | planets =
                            IdDict.insert planetId newPlanet modelAcc.planets
                        , currentSeed = newSeed
                        , score = modelAcc.score + points
                      }
                    , lost
                    )
        )
        ( { model | turns = model.turns + 1 }, False )
        model.planets


type CountdownUpdateResult
    = PlanetNotLost Planet
    | PlanetScored Int (Random.Generator Planet)
    | PlanetLost Planet
    | PlanetUnchanged


updateCountdown : PlayingModel -> Planet -> CountdownUpdateResult
updateCountdown model planet =
    case planet.kind of
        VirginPlanet _ ->
            PlanetUnchanged

        ColonyPlanet colony ->
            if colony.quantity <= 0 then
                Random.map2
                    (\quantity product ->
                        { planet
                            | kind =
                                ColonyPlanet
                                    { countdown = colony.nextCountdown
                                    , quantity = colony.nextQuantity
                                    , product = colony.nextProduct
                                    , nextCountdown = max 5 model.rings
                                    , nextQuantity = max 1 quantity
                                    , nextProduct = product
                                    }
                        }
                    )
                    (Random.int 1 model.rings)
                    (randomColonyRequest (Types.gamePhase model) model.vegetarian)
                    |> PlanetScored colony.countdown

            else if colony.countdown <= 1 then
                PlanetLost { planet | kind = ColonyPlanet { colony | countdown = 0 } }

            else
                { planet
                    | kind =
                        ColonyPlanet { colony | countdown = colony.countdown - 1 }
                }
                    |> PlanetNotLost

        OccupiedPlanet (FarmPlanet farm) ->
            { planet
                | kind =
                    OccupiedPlanet
                        (FarmPlanet { farm | countdown = max 0 (farm.countdown - 1) })
            }
                |> PlanetNotLost

        OccupiedPlanet (FactoryPlanet _) ->
            PlanetUnchanged

        OccupiedPlanet (DepositPlanet _) ->
            PlanetUnchanged


randomColonyRequest : GamePhase -> Bool -> Random.Generator Food
randomColonyRequest gamePhase vegetarian =
    case gamePhase of
        EarlyGame ->
            case
                Food.allIngredients vegetarian
                    |> List.Extra.removeWhen
                        (\ingredient ->
                            colonyNeverAsks (Food.Ingredient ingredient)
                                || Food.isDuneIngredient ingredient
                        )
            of
                [] ->
                    Random.constant (Food.Ingredient Food.Water)

                h :: t ->
                    Random.map Food.Ingredient (Random.uniform h t)

        MidGame ->
            case
                Food.all vegetarian
                    |> List.Extra.removeWhen
                        (\food ->
                            colonyNeverAsks food
                                || Food.isDuneFood food
                        )
            of
                [] ->
                    Random.constant (Food.Ingredient Food.Water)

                h :: t ->
                    Random.uniform h t

        LateGame ->
            case
                Food.all vegetarian
                    |> List.Extra.removeWhen colonyNeverAsks
            of
                [] ->
                    Random.constant (Food.Ingredient Food.Water)

                h :: t ->
                    Random.uniform h t


colonyNeverAsks : Food -> Bool
colonyNeverAsks food =
    case food of
        Food.Ingredient Food.Chicken ->
            True

        Food.Ingredient Food.Cow ->
            True

        Food.Ingredient Food.Fish ->
            True

        Food.Ingredient Food.Pig ->
            True

        _ ->
            False


addNewPlanets : PlayingModel -> PlayingModel
addNewPlanets model =
    let
        ( maximumDistanceOccupied, maximumDistanceSeen ) =
            IdDict.fold
                (\_ planet ( maxOccupied, maxSeen ) ->
                    let
                        distance : Length
                        distance =
                            Point2d.distanceFrom Point2d.origin planet.position
                    in
                    case planet.kind of
                        VirginPlanet _ ->
                            ( maxOccupied
                            , Quantity.max distance maxSeen
                            )

                        ColonyPlanet _ ->
                            ( Quantity.max distance maxOccupied
                            , Quantity.max distance maxSeen
                            )

                        OccupiedPlanet _ ->
                            ( Quantity.max distance maxOccupied
                            , Quantity.max distance maxSeen
                            )
                )
                ( Quantity.zero, Quantity.zero )
                model.planets
    in
    if
        Quantity.difference maximumDistanceSeen maximumDistanceOccupied
            |> Quantity.lessThan ringWidth
    then
        let
            toAdd : Int
            toAdd =
                maximumDistanceSeen
                    |> Length.inLightYears
                    |> (\n -> n / 2)
                    |> ceiling
                    |> (+) 10
        in
        List.foldl
            (\_ m ->
                addPlanet model.gameMode model.vegetarian 100 maximumDistanceSeen m
            )
            { model | rings = model.rings + 1 }
            (List.range 1 toAdd)

    else
        model


addPlanet : GameMode -> Bool -> Float -> Length -> PlayingModel -> PlayingModel
addPlanet mode vegetarian budget fromDistance model =
    let
        ( planet, nextSeed ) =
            Random.step planetGenerator model.currentSeed

        planetGenerator : Random.Generator PlanetGenerationResult
        planetGenerator =
            Random.map3 (Planet IdDict.empty)
                nameGenerator
                positionGenerator
                kindGenerator
                |> Random.andThen
                    (\generated ->
                        if
                            IdDict.any
                                (\_ existing ->
                                    Point2d.distanceFrom
                                        existing.position
                                        generated.position
                                        |> Quantity.lessThan minimumPlanetDistance
                                )
                                model.planets
                        then
                            Random.weighted
                                ( budget, RetryGeneration )
                                [ ( 1, GiveUpGeneration ) ]

                        else
                            Random.constant (PlanetGenerated generated)
                    )

        positionGenerator : Random.Generator (Point2d Meters ())
        positionGenerator =
            let
                from : Length
                from =
                    Quantity.max ringWidth fromDistance
            in
            Random.map2 Point2d.rTheta
                (Random.float
                    (Length.inLightYears from)
                    (Length.inLightYears (from |> Quantity.plus ringWidth))
                    |> Random.map Length.lightYears
                )
                (Random.map Angle.radians (Random.float 0 (2 * pi)))

        kindGenerator : Random.Generator PlanetKind
        kindGenerator =
            Random.map3
                (\farmOptions factoryOption depositOption ->
                    (farmOptions ++ [ factoryOption, depositOption ])
                        |> VirginPlanet
                )
                (farmGenerator (Types.gamePhase model) vegetarian 6 [])
                factoryGenerator
                (depositGenerator mode)
    in
    case planet of
        GiveUpGeneration ->
            { model | currentSeed = nextSeed }

        PlanetGenerated generated ->
            { model
                | currentSeed = nextSeed
                , planets = IdDict.insertNew generated model.planets
            }

        RetryGeneration ->
            addPlanet mode model.vegetarian (budget - 5) fromDistance { model | currentSeed = nextSeed }


depositGenerator : GameMode -> Random.Generator OccupiedPlanet
depositGenerator gameMode =
    case gameMode of
        Easy ->
            DepositPlanet
                { capacity = Nothing
                , content = Food.Dict.empty
                }
                |> Random.constant

        Normal ->
            DepositPlanet
                { capacity = Nothing
                , content = Food.Dict.empty
                }
                |> Random.constant

        Hard ->
            Random.map
                (\capacity ->
                    DepositPlanet
                        { capacity = Just capacity
                        , content = Food.Dict.empty
                        }
                )
                (Random.int 10 25)


factoryGenerator : Random.Generator OccupiedPlanet
factoryGenerator =
    Random.map
        (\efficiency ->
            FactoryPlanet
                { efficiency = efficiency
                , deposit = Food.Dict.empty
                , product = Nothing
                }
        )
        (Random.weighted ( 0.25, 0 )
            [ ( 1, 1 )
            , ( 3, 2 )
            , ( 0.5, 3 )
            ]
        )


farmGenerator :
    GamePhase
    -> Bool
    -> Int
    -> List FarmData
    -> Random.Generator (List OccupiedPlanet)
farmGenerator gamePhase vegetarian count acc =
    if count <= 0 then
        acc
            |> List.sortBy (\option -> -option.perTurn * option.countdown)
            |> List.map FarmPlanet
            |> Random.constant

    else
        Random.map3 FarmData
            (randomIngredient gamePhase vegetarian (List.map .ingredient acc))
            (Random.int 2 10)
            (Random.int 1 3)
            |> Random.andThen (\farm -> farmGenerator gamePhase vegetarian (count - 1) (farm :: acc))


randomIngredient : GamePhase -> Bool -> List Ingredient -> Random.Generator Ingredient
randomIngredient gamePhase vegetarian existing =
    case
        List.Extra.removeWhen
            (\ingredient ->
                List.member ingredient existing
                    || ((gamePhase /= LateGame)
                            && Food.isDuneIngredient ingredient
                       )
            )
            (Food.allIngredients vegetarian)
    of
        [] ->
            Random.constant Food.Water

        h :: t ->
            Random.uniform h t


nameGenerator : Random.Generator String
nameGenerator =
    -- TODO: unsteal this
    let
        pairGenerator : Random.Generator String
        pairGenerator =
            Random.uniform ""
                [ "le", "xe", "ge", "za", "ce", "bi", "so", "us", "es", "ar", "ma", "in", "di", "re", "a", "er", "at", "en", "be", "ra", "la", "ve", "ti", "ed", "or", "qu", "an", "te", "is", "ri", "on" ]
    in
    Random.weighted ( 3, 4 ) [ ( 1, 5 ) ]
        |> Random.andThen (\length -> Random.list length pairGenerator)
        |> Random.map (\l -> String.Extra.toSentenceCase (String.concat l))


type PlanetGenerationResult
    = RetryGeneration
    | PlanetGenerated Planet
    | GiveUpGeneration
