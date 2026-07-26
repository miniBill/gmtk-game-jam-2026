module Update exposing (init, update)

import Angle
import Audio exposing (AudioCmd, AudioData)
import Browser
import Browser.Dom
import Id exposing (Id(..), PlanetId)
import IdDict
import Length exposing (Length, Meters)
import List.Extra
import Pixels
import Point2d exposing (Point2d)
import Process
import Product exposing (Product(..))
import Product.Dict
import Quantity
import Random
import String.Extra
import Task
import Theme
import Time
import Turn
import Types exposing (FarmData, GameMode, Highlighted(..), Model, Msg(..), OccupiedPlanet(..), Page(..), Planet, PlanetKind(..), PlayingModel, PlayingMsg(..), Selected(..))
import View


minimumPlanetDistance : Length
minimumPlanetDistance =
    Length.lightYears 0.75


ringWidth : Length
ringWidth =
    Length.lightYears 1


init : () -> ( Model, Cmd Msg, AudioCmd Msg )
init () =
    ( { page = Menu
      , sound = Nothing
      }
    , Cmd.none
    , Audio.cmdNone
    )


update : AudioData -> Msg -> Model -> ( Model, Cmd Msg, AudioCmd Msg )
update audioData msg model =
    case {- Debug.log "msg" -} msg of
        PlaySound ->
            case model.sound of
                Nothing ->
                    ( model
                    , Cmd.none
                    , Audio.loadAudio AudioLoadResult Theme.bleep
                    )

                Just ( sound, _ ) ->
                    ( model
                    , Time.now |> Task.perform (TimeResult sound)
                    , Audio.cmdNone
                    )

        Play gameMode ->
            ( model
            , Random.int 0 Random.maxInt |> Random.generate (InitialSeed gameMode)
            , Audio.cmdNone
            )

        InitialSeed gameMode initialSeed ->
            ( { model
                | page =
                    initPlayingModel gameMode initialSeed
                        |> updatePlanets
                        |> Playing
              }
            , getSvgContainerSize
            , Audio.cmdNone
            )

        AudioLoadResult (Err e) ->
            let
                _ =
                    Debug.log "AudioLoadResult (Err e)" e
            in
            ( model, Cmd.none, Audio.cmdNone )

        AudioLoadResult (Ok sound) ->
            ( model, Time.now |> Task.perform (TimeResult sound), Audio.cmdNone )

        TimeResult sound now ->
            ( { model | sound = Just ( sound, now ) }, Cmd.none, Audio.cmdNone )

        PlayingMsg playingMsg ->
            case model.page of
                Menu ->
                    ( model, Cmd.none, Audio.cmdNone )

                Playing playing ->
                    let
                        ( newPage, cmd ) =
                            updatePlaying playingMsg playing
                    in
                    ( { model | page = newPage }
                    , Cmd.map PlayingMsg cmd
                    , Audio.cmdNone
                    )

                Lost _ ->
                    ( model, Cmd.none, Audio.cmdNone )

        Resized _ _ ->
            ( model, getSvgContainerSize, Audio.cmdNone )

        GotSvgContainerSize (Err e) ->
            let
                _ =
                    Debug.log "GotSvgContainerSize (Err e)" e
            in
            ( model, Cmd.none, Audio.cmdNone )

        GotSvgContainerSize (Ok { viewport }) ->
            case model.page of
                Menu ->
                    ( model, Cmd.none, Audio.cmdNone )

                Playing playingModel ->
                    ( { model
                        | page =
                            Playing
                                { playingModel
                                    | svgContainerSize =
                                        ( Pixels.pixels (floor viewport.width)
                                        , Pixels.pixels (floor viewport.height)
                                        )
                                }
                      }
                    , Cmd.none
                    , Audio.cmdNone
                    )

                Lost _ ->
                    ( model, Cmd.none, Audio.cmdNone )


getSvgContainerSize =
    Process.sleep 0
        |> Task.andThen (\_ -> Browser.Dom.getViewportOf View.svgContainerId)
        |> Task.attempt GotSvgContainerSize


initPlayingModel : GameMode -> Int -> PlayingModel
initPlayingModel gameMode initialSeed =
    { initialSeed = initialSeed
    , currentSeed = Random.initialSeed initialSeed
    , planets = IdDict.empty |> IdDict.insertNew initialEarth
    , selected = SelectedNone
    , highlighted = HighlightedNone
    , score = 0
    , gameMode = gameMode
    , rings = 1
    , svgContainerSize = ( Pixels.pixels 200, Pixels.pixels 200 )
    , center = Point2d.origin
    , zoom = Quantity.rate Length.lightYear (Pixels.pixels 200)
    }


initialEarth : Planet
initialEarth =
    { name = "Terra"
    , position = Point2d.origin
    , links = IdDict.empty
    , kind =
        ColonyPlanet
            { product = Water
            , quantity = 1
            , countdown = 10
            }
    }


updatePlaying : PlayingMsg -> PlayingModel -> ( Page, Cmd PlayingMsg )
updatePlaying msg model =
    case msg of
        SelectPlanet id ->
            let
                new : Selected
                new =
                    SelectedPlanet id
            in
            if new == model.selected then
                ( Playing { model | selected = SelectedNone }, Cmd.none )

            else
                ( Playing { model | selected = new }, Cmd.none )

        HighlightPlanet id ->
            ( Playing { model | highlighted = HighlightedPlanet id }, Cmd.none )

        HighlightNone ->
            ( Playing { model | highlighted = HighlightedNone }, Cmd.none )

        OccupyPlanet id kind ->
            ( Playing
                { model
                    | planets =
                        IdDict.updateIfExists
                            id
                            (\planet -> { planet | kind = OccupiedPlanet kind })
                            model.planets
                }
            , Cmd.none
            )

        EndTurn ->
            ( case Turn.run model of
                Ok newModel ->
                    Playing (updatePlanets newModel)

                Err newModel ->
                    Lost newModel
            , Cmd.none
            )

        SetFactoryProduction id order ->
            ( { model
                | planets =
                    IdDict.updateIfExists
                        id
                        (\planet ->
                            case planet.kind of
                                OccupiedPlanet (FactoryPlanet factory) ->
                                    { planet
                                        | kind =
                                            OccupiedPlanet (FactoryPlanet { factory | order = order })
                                    }

                                _ ->
                                    planet
                        )
                        model.planets
              }
                |> Playing
            , Cmd.none
            )

        SetLink from to product quantity ->
            ( { model
                | planets =
                    IdDict.updateIfExists
                        from
                        (\planet ->
                            { planet
                                | links =
                                    IdDict.update to
                                        (\links ->
                                            links
                                                |> Maybe.withDefault Product.Dict.empty
                                                |> Product.Dict.insert product quantity
                                                |> Just
                                        )
                                        planet.links
                            }
                        )
                        model.planets
              }
                |> Playing
            , Cmd.none
            )


updatePlanets : PlayingModel -> PlayingModel
updatePlanets model =
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
                (Random.map Length.lightYears
                    (Random.float
                        (Length.inLightYears from)
                        (Length.inLightYears (from |> Quantity.plus ringWidth))
                    )
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
            Random.constant Water

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
