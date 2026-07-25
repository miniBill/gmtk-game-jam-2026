module Turn exposing (run)

import Id exposing (Id, PlanetId)
import IdDict exposing (IdDict)
import Product
import Product.Dict exposing (ProductDict)
import Random
import Types exposing (LostModel, OccupiedPlanet(..), Planet, PlanetKind(..), PlayingModel)


run : PlayingModel -> Result LostModel PlayingModel
run model =
    let
        ( newModel, hasLost ) =
            model
                |> calculateExports
                |> applyImport
                |> updateCountdowns
    in
    if hasLost then
        { initialSeed = newModel.initialSeed
        , planets = newModel.planets
        , selected = newModel.selected
        , highlighted = newModel.highlighted
        , score = newModel.score
        }
            |> Err

    else
        { initialSeed = newModel.initialSeed
        , currentSeed = newModel.currentSeed
        , planets = newModel.planets
        , selected = newModel.selected
        , highlighted = newModel.highlighted
        , score = newModel.score
        }
            |> Ok


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
    let
        availableCapacity : Int
        availableCapacity =
            capacity - Product.Dict.sum content
    in
    fillDepositStep availableCapacity (Product.Dict.toList imports) [] content


fillDepositStep : Int -> List ( Product.Product, Int ) -> List ( Product.Product, Int ) -> ProductDict Int -> ProductDict Int
fillDepositStep availableCapacity queue next content =
    if availableCapacity <= 0 then
        content

    else
        case queue of
            [] ->
                case next of
                    [] ->
                        content

                    _ ->
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
    let
        maxRequired : Int
        maxRequired =
            round (toFloat (IdDict.size model.planets) ^ 0.75)
    in
    IdDict.fold
        (\planetId planet (( modelAcc, lost ) as acc) ->
            case updateCountdown maxRequired planet of
                PlanetUnchanged ->
                    acc

                PlanetNotLost newPlanetGenerator ->
                    let
                        ( newPlanet, newSeed ) =
                            Random.step newPlanetGenerator modelAcc.currentSeed
                    in
                    ( { modelAcc
                        | planets =
                            IdDict.insert planetId newPlanet model.planets
                        , currentSeed = newSeed
                      }
                    , lost
                    )

                PlanetLost newPlanet ->
                    ( { modelAcc
                        | planets = IdDict.insert planetId newPlanet model.planets
                      }
                    , True
                    )
        )
        ( model, False )
        model.planets


type CountdownUpdateResult
    = PlanetNotLost (Random.Generator Planet)
    | PlanetLost Planet
    | PlanetUnchanged


updateCountdown : Int -> Planet -> CountdownUpdateResult
updateCountdown maxRequired planet =
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
                                        | countdown = 10
                                        , quantity = quantity
                                        , product = product
                                    }
                        }
                    )
                    (Random.int 1 (max 1 maxRequired))
                    (case Product.all of
                        [] ->
                            Random.constant Product.Water

                        h :: t ->
                            Random.uniform h t
                    )
                    |> PlanetNotLost

            else if colony.countdown <= 1 then
                PlanetLost { planet | kind = ColonyPlanet { colony | countdown = 0 } }

            else
                { planet
                    | kind =
                        ColonyPlanet { colony | countdown = colony.countdown - 1 }
                }
                    |> Random.constant
                    |> PlanetNotLost

        OccupiedPlanet (FarmPlanet farm) ->
            { planet
                | kind =
                    OccupiedPlanet
                        (FarmPlanet { farm | countdown = farm.countdown - 1 })
            }
                |> Random.constant
                |> PlanetNotLost

        OccupiedPlanet (FactoryPlanet _) ->
            PlanetUnchanged

        OccupiedPlanet (DepositPlanet _) ->
            PlanetUnchanged
