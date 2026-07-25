module Turn exposing (run)

import Id exposing (PlanetId)
import IdDict exposing (IdDict)
import Product exposing (Product)
import Product.Dict
import Types exposing (OccupiedPlanet(..), PlanetKind(..), PlayingModel)


run : PlayingModel -> PlayingModel
run model =
    model
        |> production
        |> calculateExport
        |> Tuple.mapFirst cleanFactories
        |> applyExport
        |> updateCountdown


production :
    PlayingModel
    ->
        ( PlayingModel
        , IdDict PlanetId { product : Product, quantity : Int }
        )
production model =
    ( model
    , model.planets
        |> IdDict.filterMap
            (\_ planet ->
                case planet.kind of
                    VirginPlanet _ ->
                        Nothing

                    ColonyPlanet _ ->
                        Nothing

                    OccupiedPlanet (FarmPlanet farm) ->
                        Just { product = farm.product, quantity = farm.perTurn }

                    OccupiedPlanet (FactoryPlanet { order, efficiency, deposit }) ->
                        Maybe.map2
                            (\product recipe ->
                                let
                                    quantity : Int
                                    quantity =
                                        List.foldl
                                            (\recipeLine acc ->
                                                let
                                                    has : Int
                                                    has =
                                                        Product.Dict.get recipeLine.product deposit
                                                            |> Maybe.withDefault 0
                                                in
                                                min acc (has // recipeLine.quantity)
                                            )
                                            efficiency
                                            recipe
                                in
                                { product = product, quantity = quantity }
                            )
                            order
                            (Maybe.andThen Product.toRecipe order)

                    OccupiedPlanet (DepositPlanet _) ->
                        Nothing
            )
    )


calculateExport :
    ( PlayingModel
    , IdDict PlanetId { product : Product, quantity : Int }
    )
    ->
        ( PlayingModel
        , IdDict PlanetId { product : Product, quantity : Int }
        )
calculateExport model =
    Debug.todo "TODO"


cleanFactories : PlayingModel -> PlayingModel
cleanFactories model =
    Debug.todo "TODO"


applyExport :
    ( PlayingModel
    , IdDict PlanetId { product : Product, quantity : Int }
    )
    -> PlayingModel
applyExport model =
    Debug.todo "TODO"


updateCountdown : PlayingModel -> PlayingModel
updateCountdown model =
    Debug.todo "TODO"



-- 1 production (farm + factory) into tmp
-- 2 links from tmp and containers into tmp2
-- 3 clean factory inputs
-- 4 apply link result from tmp2
-- 5 update countdowns
-- 5.1 if colony need is 0, generate new need and score, else if colony countdown is zero, you die
