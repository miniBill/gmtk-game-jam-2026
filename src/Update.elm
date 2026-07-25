module Update exposing (init, update)

import Angle
import Audio exposing (AudioCmd, AudioData)
import Id exposing (Id(..), PlanetId)
import IdDict
import Length exposing (Length, Meters)
import List.Extra
import Point2d exposing (Point2d)
import Product exposing (Product(..))
import Quantity
import Random
import String.Extra
import Task
import Theme
import Time
import Types exposing (FarmData, Highlighted(..), Model, Msg(..), OccupiedPlanet(..), Page(..), Planet, PlanetKind(..), PlayingModel, PlayingMsg(..), Selected(..))


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

        Play ->
            ( model
            , Random.int 0 Random.maxInt |> Random.generate InitialSeed
            , Audio.cmdNone
            )

        InitialSeed initialSeed ->
            ( { model
                | page =
                    initPlayingModel initialSeed
                        |> updatePlanets
                        |> Playing
              }
            , Cmd.none
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
                        ( newPlaying, cmd ) =
                            updatePlaying playingMsg playing
                    in
                    ( { model | page = Playing newPlaying }
                    , Cmd.map PlayingMsg cmd
                    , Audio.cmdNone
                    )


initPlayingModel : Int -> PlayingModel
initPlayingModel initialSeed =
    { initialSeed = initialSeed
    , currentSeed = Random.initialSeed initialSeed
    , maximumDistanceReched = Quantity.zero
    , planets = IdDict.empty |> IdDict.insert initialEarth
    , selected = SelectedNone
    , highlighted = HighlightedNone
    , score = 0
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
            , timeout = 10
            }
    }


updatePlaying : PlayingMsg -> PlayingModel -> ( PlayingModel, Cmd PlayingMsg )
updatePlaying msg model =
    case msg of
        SelectPlanet id ->
            let
                new : Selected
                new =
                    SelectedPlanet id
            in
            if new == model.selected then
                ( { model | selected = SelectedNone }, Cmd.none )

            else
                ( { model | selected = new }, Cmd.none )

        HighlightLink from to ->
            ( { model | highlighted = HighlightedLink from to }, Cmd.none )

        HighlightPlanet id ->
            ( { model | highlighted = HighlightedPlanet id }, Cmd.none )

        HighlightNone ->
            ( { model | highlighted = HighlightedNone }, Cmd.none )

        OccupyPlanet id kind ->
            ( { model
                | planets =
                    IdDict.updateIfExists
                        id
                        (\planet -> { planet | kind = OccupiedPlanet kind })
                        model.planets
              }
            , Cmd.none
            )

        EndTurn ->
            ( model |> updatePlanets, Cmd.none )

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
                addPlanet 100 maximumDistanceSeen m
            )
            model
            (List.range 1 toAdd)

    else
        model


addPlanet : Float -> Length -> PlayingModel -> PlayingModel
addPlanet budget fromDistance model =
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
                depositGenerator
    in
    case planet of
        GiveUpGeneration ->
            { model | currentSeed = nextSeed }

        PlanetGenerated generated ->
            { model
                | currentSeed = nextSeed
                , planets = IdDict.insert generated model.planets
            }

        RetryGeneration ->
            addPlanet (budget - 5) fromDistance { model | currentSeed = nextSeed }


depositGenerator : Random.Generator OccupiedPlanet
depositGenerator =
    Random.map
        (\capacity ->
            DepositPlanet
                { capacity = capacity
                , content = []
                }
        )
        (Random.int 5 10)


factoryGenerator : Random.Generator OccupiedPlanet
factoryGenerator =
    Random.map
        (\efficiency ->
            FactoryPlanet
                { efficiency = efficiency
                , deposit = []
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
        Random.map3
            (\product maxTurns perTurn ->
                { product = product
                , timeout = maxTurns
                , perTurn = perTurn
                }
            )
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
