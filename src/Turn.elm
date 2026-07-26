module Turn exposing (addNewPlanets, run)

import Angle
import Id exposing (Id, PlanetId)
import IdDict exposing (IdDict)
import Length exposing (Length, Meters)
import List.Extra
import Point2d exposing (Point2d)
import Product exposing (Product)
import Product.Dict exposing (ProductDict)
import Quantity
import Random
import String.Extra
import Types exposing (FarmData, GameMode, LostModel, OccupiedPlanet(..), Planet, PlanetKind(..), PlayingModel)


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
        , planets = newModel.planets
        , selected = newModel.selected
        , highlighted = newModel.highlighted
        , score = newModel.score
        , gameMode = newModel.gameMode
        }
            |> Err

    else
        Ok newModel


calculateExports :
    PlayingModel
    ->
        ( PlayingModel
        , IdDict PlanetId (ProductDict Int)
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
    -> ( IdDict PlanetId Planet, IdDict PlanetId (ProductDict Int) )
    -> ( IdDict PlanetId Planet, IdDict PlanetId (ProductDict Int) )
calculateExport planetId planet (( accPlanets, accExports ) as acc) =
    let
        calculate :
            ProductDict Int
            ->
                ( IdDict PlanetId (ProductDict Int)
                , ProductDict Int
                )
        calculate available =
            IdDict.fold
                (\to wanted ( exporting, remaining ) ->
                    let
                        ( toAdd, newRemaining ) =
                            Product.Dict.merge
                                (\_ _ iacc -> iacc)
                                (\product want remain ( exportAcc, remainAcc ) ->
                                    let
                                        quantity : Int
                                        quantity =
                                            min want remain
                                    in
                                    ( Product.Dict.insert product quantity exportAcc
                                    , Product.Dict.insert product (remain - quantity) remainAcc
                                    )
                                )
                                (\_ _ iacc -> iacc)
                                wanted
                                remaining
                                ( Product.Dict.empty, remaining )
                    in
                    ( IdDict.insert to toAdd exporting, newRemaining )
                )
                ( IdDict.empty, available )
                planet.links

        merge : IdDict PlanetId (ProductDict Int) -> IdDict PlanetId (ProductDict Int)
        merge calculated =
            IdDict.updateWith calculated
                { inBoth =
                    \k l r iacc ->
                        IdDict.insert k
                            (Product.Dict.mergeSum l r)
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
                available : ProductDict Int
                available =
                    if farm.countdown > 0 then
                        Product.Dict.singleton farm.product farm.perTurn

                    else
                        Product.Dict.empty
            in
            ( accPlanets, merge (Tuple.first (calculate available)) )

        OccupiedPlanet (FactoryPlanet factory) ->
            let
                available : ProductDict Int
                available =
                    Maybe.map2
                        (\product recipe ->
                            let
                                quantity : Int
                                quantity =
                                    List.foldl
                                        (\recipeLine iacc ->
                                            let
                                                has : Int
                                                has =
                                                    Product.Dict.get recipeLine.product factory.deposit
                                                        |> Maybe.withDefault 0
                                            in
                                            min iacc (has // recipeLine.quantity)
                                        )
                                        factory.efficiency
                                        recipe
                            in
                            Product.Dict.singleton product quantity
                        )
                        factory.order
                        (Maybe.andThen Product.toRecipe factory.order)
                        |> Maybe.withDefault Product.Dict.empty
            in
            ( IdDict.insert planetId
                { planet
                    | kind =
                        OccupiedPlanet
                            (FactoryPlanet
                                { factory | deposit = Product.Dict.empty }
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
    , IdDict PlanetId (ProductDict Int)
    )
    -> PlayingModel
applyImport ( model, exports ) =
    { model
        | planets =
            IdDict.updateWith exports
                { inBoth =
                    \planetId planet imports acc ->
                        if Product.Dict.all (\_ q -> q == 0) imports then
                            acc

                        else
                            case planet.kind of
                                VirginPlanet _ ->
                                    acc

                                ColonyPlanet colony ->
                                    case Product.Dict.get colony.product imports of
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
                                        newContent : ProductDict Int
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


fillDepositWith : ProductDict Int -> Types.DepositData -> ProductDict Int
fillDepositWith imports { capacity, content } =
    case capacity of
        Nothing ->
            Product.Dict.mergeSum content imports

        Just total ->
            let
                availableCapacity : Int
                availableCapacity =
                    total - Product.Dict.sum content
            in
            fillDepositStep availableCapacity (Product.Dict.toList imports) [] content


fillDepositStep : Int -> List ( Product, Int ) -> List ( Product, Int ) -> ProductDict Int -> ProductDict Int
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
                        (Product.Dict.update product
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
            case updateCountdown model.rings planet of
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
        ( model, False )
        model.planets


type CountdownUpdateResult
    = PlanetNotLost Planet
    | PlanetScored Int (Random.Generator Planet)
    | PlanetLost Planet
    | PlanetUnchanged


updateCountdown : Int -> Planet -> CountdownUpdateResult
updateCountdown rings planet =
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
                                    { colony
                                        | countdown = max 5 rings
                                        , quantity = max 1 quantity
                                        , product = product
                                    }
                        }
                    )
                    (Random.int 1 rings)
                    (case Product.all of
                        [] ->
                            Random.constant Product.Water

                        h :: t ->
                            Random.uniform h t
                    )
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
                addPlanet model.gameMode 100 maximumDistanceSeen m
            )
            { model | rings = model.rings + 1 }
            (List.range 1 toAdd)

    else
        model


addPlanet : GameMode -> Float -> Length -> PlayingModel -> PlayingModel
addPlanet mode budget fromDistance model =
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
                (farmGenerator 3 [])
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
            addPlanet mode (budget - 5) fromDistance { model | currentSeed = nextSeed }


depositGenerator : GameMode -> Random.Generator OccupiedPlanet
depositGenerator { hard } =
    if hard then
        Random.map
            (\capacity ->
                DepositPlanet
                    { capacity = Just capacity
                    , content = Product.Dict.empty
                    }
            )
            (Random.int 10 25)

    else
        Random.constant
            (DepositPlanet
                { capacity = Nothing
                , content = Product.Dict.empty
                }
            )


factoryGenerator : Random.Generator OccupiedPlanet
factoryGenerator =
    Random.map
        (\efficiency ->
            FactoryPlanet
                { efficiency = efficiency
                , deposit = Product.Dict.empty
                , order = Nothing
                }
        )
        (Random.weighted ( 0.25, 0 )
            [ ( 1, 1 )
            , ( 3, 2 )
            , ( 0.5, 3 )
            ]
        )


farmGenerator :
    Int
    -> List FarmData
    -> Random.Generator (List OccupiedPlanet)
farmGenerator count acc =
    if count <= 0 then
        Random.constant (List.map FarmPlanet acc)

    else
        Random.map3 FarmData
            (randomProduct (List.map .product acc))
            (Random.int 2 10)
            (Random.int 1 3)
            |> Random.andThen (\farm -> farmGenerator (count - 1) (farm :: acc))


randomProduct : List Product -> Random.Generator Product
randomProduct existing =
    case
        List.Extra.removeWhen
            (\product -> List.member product existing)
            Product.primary
    of
        [] ->
            Random.constant Product.Water

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
